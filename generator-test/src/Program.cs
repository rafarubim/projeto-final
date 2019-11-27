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
        triggersLst = triggersLst.Add(new Trigger("tick", 5));
        triggersLst = triggersLst.Add(new Trigger("tick", 15));
        var triggeredEvents = gen.QueryGenerator(15, triggersLst);

        var statesSet = gen.States;
      }
    }
  }
}
