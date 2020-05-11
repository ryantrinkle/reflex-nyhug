import Benchmark.Backend (benchmarks)
import Criterion.Main

main :: IO ()
main = defaultMain benchmarks
