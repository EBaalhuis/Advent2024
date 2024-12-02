namespace Advent2024Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.Day1Part1 () =
        let expected = 1189304
        let actual = Advent2024.Day1Part1.result
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.Day1Part2 () =
        let expected = 24349736
        let actual = Advent2024.Day1Part2.result
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.Day2Part1 () =
        let expected = 279
        let actual = Advent2024.Day2Part1.result
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.Day2Part2 () =
        let expected = 343
        let actual = Advent2024.Day2Part2.result
        Assert.AreEqual(expected, actual);
