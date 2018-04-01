{-# LANGUAGE ExistentialQuantification #-}

module EventSpring.Projection (
    IsProjectionIdFor,

    CanHandleProjection,
    initialProjection,

    CanHandleEvent,
    changesForEvent,

    Update(..)
) where

import EventSpring.Internal.Projection
