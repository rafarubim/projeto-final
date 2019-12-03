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
        gen.UserPersonality = new UserPersonality(0.7f,0.5f,0.1f,0.8f,-0.1f);

        Console.WriteLine("User personality Inputed:");
        var personality = gen.UserPersonality;
        Console.WriteLine($"Openess: {personality.Openess}");
        Console.WriteLine($"Conscientiousness: {personality.Conscientiousness}");
        Console.WriteLine($"Extraversion: {personality.Extraversion}");
        Console.WriteLine($"Agreeableness: {personality.Agreeableness}");
        Console.WriteLine($"Neuroticism: {personality.Neuroticism}");
        Console.WriteLine("");

        var triggersLst = ImmutableList.Create<Trigger>();
        var triggeredEvents = gen.Query(5, triggersLst);

        Console.WriteLine("tick(1) <---------------------------------------------------------------");
        
        foreach(var ev in triggeredEvents)
        {
          Console.Write("Name: ");
          Console.Write(ev.Name);
          Console.Write("/ Arity: ");
          Console.Write(ev.Arity);
          Console.Write("/ OcurrenceTime: ");
          Console.WriteLine(ev.OcurrenceTime);

          Console.Write("Args: (");
          foreach (var arg in ev.Args)
          {
            Console.Write((Entity)arg.Term);
            Console.Write(" , ");
          }
          Console.WriteLine(")");
          Console.WriteLine();
        }

        triggersLst = ImmutableList.Create<Trigger>();
        triggersLst = triggersLst.Add(new Trigger("villainActs", 5));
        triggeredEvents = gen.Query(10, triggersLst);

        Console.WriteLine("villainActs(5) <---------------------------------------------------------------");

        foreach (var ev in triggeredEvents)
        {
          Console.Write("Name: ");
          Console.Write(ev.Name);
          Console.Write("/ Arity: ");
          Console.Write(ev.Arity);
          Console.Write("/ OcurrenceTime: ");
          Console.WriteLine(ev.OcurrenceTime);

          Console.Write("Args: (");
          foreach (var arg in ev.Args)
          {
            Console.Write((Entity)arg.Term);
            Console.Write(" , ");
          }
          Console.WriteLine(")");
          Console.WriteLine();
        }

        triggersLst = ImmutableList.Create<Trigger>();
        triggersLst = triggersLst.Add(new Trigger("heroActs", 10));
        triggeredEvents = gen.Query(15, triggersLst);
        Console.WriteLine("heroActs(10) <---------------------------------------------------------------");

        foreach (var ev in triggeredEvents)
        {
          Console.Write("Name: ");
          Console.Write(ev.Name);
          Console.Write("/ Arity: ");
          Console.Write(ev.Arity);
          Console.Write("/ OcurrenceTime: ");
          Console.WriteLine(ev.OcurrenceTime);

          Console.Write("Args: (");
          foreach (var arg in ev.Args)
          {
            Console.Write((Entity)arg.Term);
            Console.Write(" , ");
          }
          Console.WriteLine(")");
          Console.WriteLine();
        }
        
        var statesSet = gen.States;
        Console.WriteLine("States  <---------------------------------------------------------------");
        foreach (var st in statesSet)
        {
          Console.Write("Name: ");
          Console.Write(st.Name);
          Console.Write("/ Arity: ");
          Console.WriteLine(st.Arity);
          Console.Write("Args: (");
          foreach (var arg in st.Args)
          {
            if (arg.Type == typeof(Entity))
            {
              Console.Write((Entity)arg.Term);
            }
            else
            {
              Console.Write(((Scalar)arg.Term).Value);
            }
            Console.Write(" , ");
          }
          Console.WriteLine(")");
          Console.WriteLine();
        }
        
        var termArgs = new StateTerm[]
        {
          new StateTerm(new Entity("cassandra")),
          new StateTerm(new Entity("forest")),
          new StateTerm(new Entity("palace"))
        };
        gen.ExecuteEvent("move", termArgs);

        statesSet = gen.States;
        Console.WriteLine("States after forced \"move(cassandra, forest, palace)\" <---------------------------------------------------------------");
        foreach (var st in statesSet)
        {
          Console.Write("Name: ");
          Console.Write(st.Name);
          Console.Write("/ Arity: ");
          Console.WriteLine(st.Arity);
          Console.Write("Args: (");
          foreach (var arg in st.Args)
          {
            if (arg.Type == typeof(Entity))
            {
              Console.Write((Entity)arg.Term);
            }
            else
            {
              Console.Write(((Scalar)arg.Term).Value);
            }
            Console.Write(" , ");
          }
          Console.WriteLine(")");
          Console.WriteLine();
        }
      }
    }
  }
}
