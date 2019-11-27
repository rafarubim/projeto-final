using SbsSW.SwiPlCs;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace StoryGenerator.Utils
{
  static class PlTermExtension
  {
    public static PlTerm PlList(IEnumerable<PlTerm> terms)
    {
      var list = new PlTerm("[]");
      foreach (var term in terms)
      {
        list.Add(term);
      }
      return list;
    }

    public static ImmutableList<PlTerm> ArgsLst(PlTerm compoundTerm)
    {
      var argsLst = ImmutableList.Create<PlTerm>();

      for(int i = 1; i <= compoundTerm.Arity; i++)
      {
        argsLst = argsLst.Add(compoundTerm[i]);
      }

      return argsLst;
    }
  }
}
