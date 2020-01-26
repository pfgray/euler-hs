module Twelve where

import Data.List
import Primes
import Numbers

twelve = find (\n -> divisorCount n > 500) triangles

