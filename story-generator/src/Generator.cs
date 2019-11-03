using SbsSW.SwiPlCs;
using System;
using System.Diagnostics;
using System.IO;
using System.Reflection;

namespace StoryGenerator
{
  public class Generator: IDisposable
  {
    private static Generator _instance;

    private Generator()
     {
      string assemblyLoc = Assembly.GetEntryAssembly().Location;
      string assemblyDir = Path.GetDirectoryName(assemblyLoc);
      string swiplDir = Path.Combine(assemblyDir, "lib\\swipl");
      Directory.SetCurrentDirectory(swiplDir);
      Environment.SetEnvironmentVariable("SWI_HOME_DIR", swiplDir);

      Debug.Assert(!PlEngine.IsInitialized);

      // "-q" suppresses informational and banner messages
      string[] param = { "-q" };
      PlEngine.Initialize(param);
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
