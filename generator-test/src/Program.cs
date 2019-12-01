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
        var triggeredEvents = gen.Query(1, triggersLst);

        gen.UserPersonality = new UserPersonality(1,0,1,0,1);

        Console.WriteLine("User personality:  -----------------");
        var personality = gen.UserPersonality;
        Console.WriteLine($"Openess: {personality.Openess}");
        Console.WriteLine($"Conscientiousness: {personality.Conscientiousness}");
        Console.WriteLine($"Extraversion: {personality.Extraversion}");
        Console.WriteLine($"Agreeableness: {personality.Agreeableness}");
        Console.WriteLine($"Neuroticism: {personality.Neuroticism}");

        Console.WriteLine("Events triggered:  -----------------");
        
        foreach(var ev in triggeredEvents)
        {
          Console.Write("Name: ");
          Console.WriteLine(ev.Name);
          Console.Write("Arity: ");
          Console.WriteLine(ev.Arity);
          foreach(var arg in ev.Args)
          {
            Console.Write("Arg: ");
            Console.WriteLine((Entity)arg.Term);
          }
          Console.Write("OcurrenceTime: ");
          Console.WriteLine(ev.OcurrenceTime);
          Console.WriteLine();
        }
        /*
        var statesSet = gen.States;
        Console.WriteLine("States:  --------------------------");
        foreach (var st in statesSet)
        {
          Console.Write("Name: ");
          Console.WriteLine(st.Name);
          Console.Write("Arity: ");
          Console.WriteLine(st.Arity);
          foreach (var arg in st.Args)
          {
            Console.Write("Arg: ");
            Console.WriteLine((Entity)arg.Term);
          }
          Console.WriteLine();
        }
        
        var termArgs = new StateTerm[]
        {
          new StateTerm(new Entity("cassandra")),
          new StateTerm(new Entity("capital")),
          new StateTerm(new Entity("palace"))
        };
        gen.ExecuteEvent("move", termArgs);

        statesSet = gen.States;
        Console.WriteLine("States after forced event:  --------------------------");
        foreach (var st in statesSet)
        {
          Console.Write("Name: ");
          Console.WriteLine(st.Name);
          Console.Write("Arity: ");
          Console.WriteLine(st.Arity);
          foreach (var arg in st.Args)
          {
            Console.Write("Arg: ");
            Console.WriteLine((Entity)arg.Term);
          }
          Console.WriteLine();
        }
        */
      }
    }
  }
}
