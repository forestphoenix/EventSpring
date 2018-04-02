{-# LANGUAGE ExistentialQuantification #-}

module EventSpring.Projection (
    ProjectionVersion,
    versionZero,

    IsProjectionIdFor,

    CanHandleProjection,
    initialProjection,

    CanHandleEvent,
    changesForEvent,

    Update(..)
) where

import EventSpring.Internal.Projection
import EventSpring.Internal.Common
