{-# LANGUAGE OverloadedStrings #-}

module Lib where

newtype Identity a = Identity {runIdentity :: a}