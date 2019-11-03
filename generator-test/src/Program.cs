using StoryGenerator;
using System;

namespace GeneratorTest
{
  class Program
  {
    static void Main(string[] args)
    {
      using (var gen = Generator.Instance)
      {
        gen.Print();
      }
    }
  }
}
