using System;
using System.Collections.Generic;
using System.Linq;

namespace StoryGenerator.Utils
{
  static class IEnumerableExtensions
  {
    public static object ToTuple<T>(this IEnumerable<T> collection)
    {
      Type genericType = Type.GetType($"System.Tuple`{collection.Count()}");
      Type[] typeArgs = collection.Select(_ => typeof(T)).ToArray();
      Type specificType = genericType.MakeGenericType(typeArgs);
      object[] constructorArguments = collection.Cast<object>().ToArray();
      return Activator.CreateInstance(specificType, constructorArguments);
    }
  }
}
