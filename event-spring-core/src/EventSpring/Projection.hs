{-# LANGUAGE ExistentialQuantification #-}

module EventSpring.Projection (
    ProjectionVersion,
    versionZero,
    mkVersion,

    IsProjectionIdFor,

    CanHandleProjection,
    initialProjection,

    CanHandleEvent,
    changesForEvent,

    Update(..)
) where

import EventSpring.Internal.Projection
import EventSpring.Internal.Common
