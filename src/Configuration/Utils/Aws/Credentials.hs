-- Copyright (c) 2013-2015 PivotCloud, Inc.
--
-- Configuration.Utils.Aws.Credentials
--
-- Please feel free to contact us at licensing@pivotmail.com with any
-- contributions, additions, or other feedback; we would love to hear from
-- you.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Configuration.Utils.Aws.Credentials
-- Copyright: Copyright © 2013-2015 PivotCloud, Inc.
-- License: Apache-2.0
-- Maintainer: Jon Sterling <jsterling@alephcloud.com>
-- Stability: experimental
--
module Configuration.Utils.Aws.Credentials
( -- * Configuration Types
  CredentialConfigKey(..)
, credentialConfigKeyId
, credentialConfigKeySecret
, validateCredentialConfigKey
, CredentialConfigFile(..)
, credentialConfigFileName
, credentialConfigKeyName
, validateCredentialConfigFile
, CredentialConfig(..)
, credentialConfigKey
, credentialConfigFile
, credentialConfigEnvironment
, credentialConfigInstanceMetadata
, defaultCredentialConfig
, pCredentialConfig
, pCredentialConfig_
, validateCredentialConfig

  -- * Load Credentials
, credentialsFromConfig
, LoadCredentialsException(..)
) where

import Aws

import Configuration.Utils
import Configuration.Utils.Internal
import Configuration.Utils.Validation

import Control.Exception
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Unicode

import Data.Maybe
import Data.Monoid.Unicode
import Data.Traversable
import Data.Typeable

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import Prelude.Unicode

-- -------------------------------------------------------------------------- --
-- Credential Configuration

data CredentialConfigKey
  = CredentialConfigKey
  { _credentialConfigKeyId ∷ !B8.ByteString
  , _credentialConfigKeySecret ∷ !B8.ByteString
  } deriving (Show, Read, Eq, Ord, Typeable)

credentialConfigKeyId ∷ Lens' CredentialConfigKey B8.ByteString
credentialConfigKeyId = lens _credentialConfigKeyId $ \a b → a { _credentialConfigKeyId = b }

credentialConfigKeySecret ∷ Lens' CredentialConfigKey B8.ByteString
credentialConfigKeySecret = lens _credentialConfigKeySecret $ \a b → a { _credentialConfigKeySecret = b }

validateCredentialConfigKey ∷ ConfigValidation CredentialConfigKey λ
validateCredentialConfigKey CredentialConfigKey{..} = do
  validateNonEmpty "access_key_id" _credentialConfigKeyId
  validateNonEmpty "access_key_secret" _credentialConfigKeySecret

instance ToJSON CredentialConfigKey where
  toJSON CredentialConfigKey{..} = object
    [ "access_key_id" .= B8.unpack _credentialConfigKeyId
    , "access_key_secret" .= B8.unpack _credentialConfigKeySecret
    ]

instance FromJSON CredentialConfigKey where
  parseJSON =
    withObject "CredentialConfigKey" $ \o →
      CredentialConfigKey
        <$> (B8.pack <$> o .: "access_key_id")
        <*> (B8.pack <$> o .: "access_key_secret")

data CredentialConfigFile
  = CredentialConfigFile
  { _credentialConfigFileName ∷ !FilePath
  , _credentialConfigKeyName ∷ !T.Text
      -- ^ default value is @"default"@
  } deriving (Show, Read, Eq, Ord, Typeable)

credentialConfigFileName ∷ Lens' CredentialConfigFile FilePath
credentialConfigFileName = lens _credentialConfigFileName $ \a b → a { _credentialConfigFileName = b }

credentialConfigKeyName ∷ Lens' CredentialConfigFile T.Text
credentialConfigKeyName = lens _credentialConfigKeyName $ \a b → a { _credentialConfigKeyName = b }

validateCredentialConfigFile ∷ ConfigValidation CredentialConfigFile λ
validateCredentialConfigFile CredentialConfigFile{..} =
  validateFileReadable "file_name" _credentialConfigFileName

instance ToJSON CredentialConfigFile where
  toJSON CredentialConfigFile{..} = object
    [ "file_name" .= _credentialConfigFileName
    , "key_name" .= _credentialConfigKeyName
    ]

instance FromJSON CredentialConfigFile where
  parseJSON =
    withObject "CredentialConfigFile" $ \o →
      CredentialConfigFile
        <$> o .: "file_name"
        <*> o .:? "key_name" .!= "default"


-- | When parsed from configuration files or the command line the settings take
-- the following precedence:
--
-- Explicit credential configuration gets precedence over credential file
-- configuration which has precedence over loading the credentials from the
-- environment over loading the credentials from the instance meta data.
--
-- Any setting on the command line always has precedence over any setting in a
-- configuration file.
--
-- If the configured file does not exist an parse error is triggered. Set this
-- value of '_credentialConfigFile' to 'Nothing' if you don't want to suppress
-- this error.
--
data CredentialConfig
  = CredentialConfig
  { _credentialConfigKey ∷ !(Maybe CredentialConfigKey)
  , _credentialConfigFile ∷ !(Maybe CredentialConfigFile)
  , _credentialConfigEnvironment ∷ !Bool
  , _credentialConfigInstanceMetadata ∷ !Bool
  } deriving (Show, Eq, Ord)

credentialConfigKey ∷ Lens' CredentialConfig (Maybe CredentialConfigKey)
credentialConfigKey = lens _credentialConfigKey $ \a b → a { _credentialConfigKey = b}

credentialConfigFile ∷ Lens' CredentialConfig (Maybe CredentialConfigFile)
credentialConfigFile = lens _credentialConfigFile $ \a b → a { _credentialConfigFile = b}

credentialConfigEnvironment ∷ Lens' CredentialConfig Bool
credentialConfigEnvironment = lens _credentialConfigEnvironment $ \a b → a { _credentialConfigEnvironment = b}

credentialConfigInstanceMetadata ∷ Lens' CredentialConfig Bool
credentialConfigInstanceMetadata = lens _credentialConfigInstanceMetadata $ \a b → a { _credentialConfigInstanceMetadata = b}

defaultCredentialConfig ∷ CredentialConfig
defaultCredentialConfig = CredentialConfig
  { _credentialConfigKey = Nothing
  , _credentialConfigFile = Nothing
  , _credentialConfigEnvironment = False
  , _credentialConfigInstanceMetadata = False
  }

validateCredentialConfig ∷ ConfigValidation CredentialConfig λ
validateCredentialConfig CredentialConfig{..} = do
  void $ for _credentialConfigKey validateCredentialConfigKey
  void $ for _credentialConfigFile validateCredentialConfigFile
  unless (or enabledList) $
    throwError "at least one credentials configuration option must be configured"

  where
    enabledList =
      [ isJust _credentialConfigKey
      , isJust _credentialConfigFile
      , _credentialConfigEnvironment
      , _credentialConfigInstanceMetadata
      ]

instance ToJSON CredentialConfig where
  toJSON CredentialConfig{..} = object
    [ "key" .= _credentialConfigKey
    , "file" .= _credentialConfigFile
    , "environment" .= _credentialConfigEnvironment
    , "metadata" .= _credentialConfigInstanceMetadata
    ]

instance FromJSON (CredentialConfig → CredentialConfig) where
  parseJSON =
    withObject "CredentialConfig" $ \o → id
      <$< credentialConfigKey ..: "key" % o
      <*< credentialConfigFile ..: "file" % o
      <*< credentialConfigEnvironment ..: "environment" % o
      <*< credentialConfigInstanceMetadata ..: "metadata" % o

-- | A command-line argument parser for 'CredentialConfig'. The @prefix@
-- argument will be prepended directly onto the argument names.
--
pCredentialConfig
  ∷ String -- ^ prefix
  → MParser CredentialConfig
pCredentialConfig prefix = id
  <$< credentialConfigKey .:: fmap Just % pCredentialConfigKey
  <*< credentialConfigFile .:: fmap Just % pCredentialConfigFile
  <*< credentialConfigEnvironment .:: boolOption
      % long (prefix ⊕ "credentials-from-environment")
      ⊕ help "load AWS access credentials from environment variables (AWS_ACCESS_KEY_ID and AWS_ACCESS_KEY_SECRET)"
  <*< credentialConfigInstanceMetadata .:: boolOption
      % long (prefix ⊕ "credentials-from-metadata")
      ⊕ help "load AWS access credentials from the instance metadata"

  where
    pKeyId =
      B8.pack <$> strOption
        % long (prefix ⊕ "credentials-key-id")
        ⊕ metavar "AWS_ACCESS_KEY_ID"
        ⊕ help "Id of the AWS access key"

    pKeySecret =
      B8.pack <$> strOption
        % long (prefix ⊕ "credentials-key-secret")
        ⊕ metavar "AWS_ACCESS_KEY_SECRET"
        ⊕ help "Secret of the AWS access key"

    pCredentialConfigKey =
      CredentialConfigKey <$> pKeyId <*> pKeySecret

    pKeyName =
      T.pack <$> strOption
        % long (prefix ⊕ "credentials-key-name")
        ⊕ metavar "STRING"
        ⊕ help "the name of the access key in the access key file"
        ⊕ value (T.unpack credentialsDefaultKey)

    pFileName =
      fileOption
        % long (prefix ⊕ "credentials-key-file")
        ⊕ help "the name of the file with access keys for the AWS API"

    pCredentialConfigFile =
      CredentialConfigFile <$> pFileName <*> pKeyName

-- | This is 'pCredentialConfig' with an empty prefix.
--
pCredentialConfig_ ∷ MParser CredentialConfig
pCredentialConfig_ = pCredentialConfig ""

data LoadCredentialsException
  = LoadCredentialsFromFileFailed CredentialConfigFile
  | LoadCredentialsFromEnvironementFailed
  | LoadCredentialsFromInstanceMetadataFailed
  | NoCredentialLoadingMethodSpecified
  deriving (Eq, Show, Typeable)

instance Exception LoadCredentialsException

-- | Load Credentials according to the credential configuration
--
-- Credentials are loaded with a precedence according to the order of
-- the record fields of 'CredentialConfig':
--
-- 1. '_credentialConfigKey', loads the explicitely specified credentials,
--
-- 2. '_credentialConfigFile', loads credentials with the given key name
--    from the given file,
--
-- 3. '_credentialConfigEnvironment', loads credentials from the
--    environment varialbes @AWS_ACCESS_KEY_ID@ and @AWS_ACCESS_KEY_SECRET@), and
--
-- 3. '_credentialConfigInstanceMetadata', load the credentials from the
--    EC2 instance metadata.
--
-- It is an error if a particular method is specified in the configuration but
-- loading the credentials with that methods fails.
--
credentialsFromConfig
  ∷ (MonadError LoadCredentialsException m, MonadIO m)
  ⇒ CredentialConfig
  → m Credentials
credentialsFromConfig CredentialConfig{..}
  | Just CredentialConfigKey{..} ← _credentialConfigKey =
      liftIO $ makeCredentials _credentialConfigKeyId _credentialConfigKeySecret

  | Just ccf@(CredentialConfigFile file awsKeyName) ← _credentialConfigFile =
      liftIO (loadCredentialsFromFile file awsKeyName) ≫= \case
        Nothing → throwError $ LoadCredentialsFromFileFailed ccf
        Just c → return c

  | _credentialConfigEnvironment =
      liftIO loadCredentialsFromEnv ≫= \case
        Nothing → throwError LoadCredentialsFromEnvironementFailed
        Just c → return c

  | _credentialConfigInstanceMetadata =
      liftIO loadCredentialsFromInstanceMetadata ≫= \case
        Nothing → throwError LoadCredentialsFromInstanceMetadataFailed
        Just c → return c

  | otherwise = throwError NoCredentialLoadingMethodSpecified

