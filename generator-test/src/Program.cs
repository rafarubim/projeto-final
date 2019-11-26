using StoryGenerator;
using System;

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
        gen.Test();
      }
    }
  }
}
