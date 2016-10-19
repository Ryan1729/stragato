module CommonUpdate exposing (..)

import Random.Pcg as Random exposing (Seed)


getSeed model time =
    { model
        | seed =
            Random.initialSeed
                <| Debug.log "seed"
                <| round time
    }
        ! []
