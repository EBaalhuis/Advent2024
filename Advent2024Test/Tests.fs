namespace Advent2024Test

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

    [<TestMethod>]
    member this.Day3Part1 () =
        let expected = 178538786
        let actual = Advent2024.Day3Part1.result
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.Day3Part2 () =
        let expected = 102467299
        let actual = Advent2024.Day3Part2.result
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.Day4Part1 () =
        let expected = 2530
        let actual = Advent2024.Day4Part1.result
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.Day4Part2 () =
        let expected = 1921
        let actual = Advent2024.Day4Part2.result
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.Day5Part1 () =
        let expected = 4135
        let actual = Advent2024.Day5Part1.result
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.Day5Part2 () =
        let expected = 5285
        let actual = Advent2024.Day5Part2.result
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.Day6Part1 () =
        let expected = 4776
        let actual = Advent2024.Day6Part1.result
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.Day6Part2 () =
        let expected = 1586
        let actual = Advent2024.Day6Part2.result
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.Day7Part1 () =
        let expected = 1708857123053L
        let actual = Advent2024.Day7Part1.result
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.Day7Part2 () =
        let expected = 189207836795655L
        let actual = Advent2024.Day7Part2.result
        Assert.AreEqual(expected, actual);

