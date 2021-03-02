-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | JSON deriving options specific to Algorand.
--
-- Interestingly, different types in the algod API use differet style
-- for field names, so we have to provide multiple options.
module Network.Algorand.Node.Api.Json
  ( algorandSnakeOptions
  , algorandTrainOptions
  ) where

import Data.Aeson.Casing (aesonPrefix, snakeCase, trainCase)
import Data.Aeson.TH (Options)


-- | Options for types using snake case.
algorandSnakeOptions :: Options
algorandSnakeOptions = aesonPrefix snakeCase

-- | Options for typees using train case.
algorandTrainOptions :: Options
algorandTrainOptions = aesonPrefix trainCase
