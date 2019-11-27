using StoryGenerator;
using System;
using System.Collections.Immutable;
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
        var triggersLst = ImmutableList.Create<Trigger>();
        triggersLst.Add(new Trigger("abc", 0));
        triggersLst.Add(new Trigger("cba", 5));
        gen.QueryGenerator(0, triggersLst);
        var statesSet = gen.States;
        foreach(var state in statesSet)
        {
          Console.WriteLine(state.Name);
        }
      }
    }
  }
}
