using SbsSW.SwiPlCs;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;

namespace StoryGenerator
{
  public class Generator : IDisposable
  {
    private static Generator _instance;
    private const string _EventProcesserLocation = "src/prolog/eventProcesser.pl";
    private const string _IndexLocation = "src/prolog/index.pl";

    public static string GenreSpecsFileLocation { get; set; }
    public static string StorySpecsFileLocation { get; set; }

    private Generator()
    {
      string currentLoc = Directory.GetCurrentDirectory();
      string assemblyLoc = Assembly.GetEntryAssembly().Location;
      string assemblyDir = Path.GetDirectoryName(assemblyLoc);
      string swiplDir = Path.Combine(assemblyDir, "lib\\swipl");
      Directory.SetCurrentDirectory(swiplDir);
      Console.WriteLine(swiplDir);
      Environment.SetEnvironmentVariable("SWI_HOME_DIR", swiplDir);

      Debug.Assert(!PlEngine.IsInitialized);

      // "-q" suppresses informational and banner messages
      string[] param = { "-q", "-f", $"../../{_EventProcesserLocation}" };
      PlEngine.Initialize(param);

      Directory.SetCurrentDirectory(currentLoc);
      var newDirQuery = @"working_directory(Dir,Dir),
atom_concat(NewDir, 'lib/swipl/', Dir),
working_directory(_, NewDir)
";
      PlQuery.PlCall(newDirQuery);
      PlQuery.PlCall($"use_module(\"{_IndexLocation}\")");

      if (GenreSpecsFileLocation == null || StorySpecsFileLocation == null)
      {
        throw new MissingMemberException();
      }
      PlQuery.PlCall(@"
beginEnumsDefinition,
beginEntityTypesDefinition,
beginStateTypesDefinition,
beginTriggerTypesDefinition,
beginEventTypesDefinition.
");
      PlQuery.PlCall($"[\"{GenreSpecsFileLocation}\"]");
      PlQuery.PlCall(@"
endEnumsDefinition,
endEntityTypesDefinition,
endStateTypesDefinition,
endTriggerTypesDefinition,
endEventTypesDefinition.
");

      PlQuery.PlCall(@"
beginEntitiesDefinition,
beginStatesDefinition,
beginTriggersDefinition,
beginEventsDefinition,
beginPlotDefinition.
");
      PlQuery.PlCall($"[\"{StorySpecsFileLocation}\"]");
      PlQuery.PlCall(@"
endEntitiesDefinition,
endStatesDefinition,
endTriggersDefinition,
endEventsDefinition,
endPlotDefinition.
");

      PlQuery.PlCall("beginEventProcesser");
    }

    public static Generator Instance
    {
      get
      {
        return _instance = _instance ?? new Generator();
      }
    }

    public void Dispose()
    {
      PlEngine.PlCleanup();
      _instance = null;
    }

    public void Test()
    {
      PlQuery.PlCall("query(12, [])");
      var q2 = new PlQuery("call(state:allStates(S))");
      Console.WriteLine("Res2: " + q2.SolutionVariables.FirstOrDefault()["S"]);
    }

    public void QueryGenerator(int currentTime, IEnumerable<(String, int)> triggers)
    {

    }

    public void Print()
    {
      // Example
      PlQuery.PlCall("assert(father(martin, inka))");
      PlQuery.PlCall("assert(father(uwe, gloria))");
      PlQuery.PlCall("assert(father(uwe, melanie))");
      PlQuery.PlCall("assert(father(uwe, ayala))");
      using (PlQuery q = new PlQuery("father(P, C), atomic_list_concat([P,' is_father_of ',C], L)"))
      {
        foreach (PlQueryVariables v in q.SolutionVariables)
          Console.WriteLine(v["L"].ToString());

        Console.WriteLine("all child's from uwe:");
        q.Variables["P"].Unify("uwe");
        foreach (PlQueryVariables v in q.SolutionVariables)
          Console.WriteLine(v["C"].ToString());
      }
      // End Example
    }
  }
}
