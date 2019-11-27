using StoryGenerator;
using System;
using System.Collections.Generic;
using System.Linq;

namespace GeneratorTest
{
  class Program
  {
    static void Main(string[] args)
    {
      Generator.GenreSpecsFileLocation = "src/medievalSpec.pl";
      Generator.StorySpecsFileLocation = "src/storySpec.pl";
      using (var gen = Generator.Instance)
      {
        //gen.Print();
        //var dict = gen.TriggerTypes();
        var testDict = new Dictionary<string, int>();
        testDict.Add("abc", 0);
        testDict.Add("cba", 5);
        gen.QueryGenerator(0, testDict);
        var statesSet = gen.States();
        var filteredStates = statesSet.Where(state => state.Args.Any(arg => arg.Type == typeof(State)));
        foreach(var state in filteredStates)
        {
          Console.WriteLine(((Entity)((State)state.Args.FirstOrDefault(arg => arg.Type == typeof(State)).Term).Args[1].Term));
        }
      }
    }
  }
}
