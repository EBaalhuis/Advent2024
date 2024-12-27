namespace Advent2024
    module Memoize =
    // jindraivanek.hashnode.dev

        let memoize f =
            let cache = System.Collections.Concurrent.ConcurrentDictionary()
            fun x -> cache.GetOrAdd(x, lazy f x).Value

        let memoizeRec f =
            let cache = System.Collections.Concurrent.ConcurrentDictionary()
            let rec recF x =
                cache.GetOrAdd(x, lazy f recF x).Value
            recF