using SbsSW.SwiPlCs;
using StoryGenerator.Utils;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Globalization;
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

    private ImmutableDictionary<StateType, ImmutableHashSet<ImmutableList<Type>>> _stateTypeSpecs = null;
    private ImmutableHashSet<string> _entities = null;

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
beginHeuristicPredicateDefinition,
beginPlotDefinition.
");
      PlQuery.PlCall($"[\"{StorySpecsFileLocation}\"]");
      PlQuery.PlCall(@"
endEntitiesDefinition,
endStatesDefinition,
endTriggersDefinition,
endEventsDefinition,
endHeuristicPredicateDefinition,
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

    private Type typeByArgName(string argName)
    {
      switch (argName)
      {
        case "entityArg":
          return typeof(Entity);
        case "stateArg":
          return typeof(State);
        case "eventArg":
          return typeof(Event);
        case "scalarArg":
          return typeof(Scalar);
        default:
          return null;
      }
    }

    private ImmutableDictionary<StateType, ImmutableHashSet<ImmutableList<Type>>> _StateTypeSpecs
    {
      get
      {
        if (_stateTypeSpecs != null)
        {
          return _stateTypeSpecs;
        }
        var stateTypeSpecColl = PlQuery.PlCallQuery("allSignatures(Signatures)").AsEnumerable();
        var stateTypeDict = ImmutableDictionary.Create<StateType, ImmutableHashSet<ImmutableList<Type>>>();

        foreach (var stateTypeSpec in stateTypeSpecColl)
        {
          var argSpecs = stateTypeSpec[2].ToList();
          var stateType = new StateType(stateTypeSpec[1].ToString(), argSpecs.Count());
          var typesLst = ImmutableList.Create<Type>();
          foreach (var spec in argSpecs)
          {
            var type = typeByArgName(spec.Name);
            typesLst = typesLst.Add(type);
          }
          if (stateTypeDict.ContainsKey(stateType))
          {
            var specsSet = stateTypeDict[stateType];
            specsSet = specsSet.Add(typesLst);
            stateTypeDict = stateTypeDict.SetItem(stateType, specsSet);
          }
          else
          {
            var specsSet = ImmutableHashSet.Create(typesLst);
            stateTypeDict = stateTypeDict.Add(stateType, specsSet);
          }
        }
        return _stateTypeSpecs = stateTypeDict;
      }
    }

    private ImmutableHashSet<string> _Entities
    {
      get
      {
        if (_entities != null)
        {
          return _entities;
        }
        var entities = ImmutableHashSet.Create<string>();

        var entsColl = PlQuery.PlCallQuery("allEntities(Entities)").AsEnumerable();
        foreach (var ent in entsColl)
        {
          entities = entities.Add(ent.ToString());
        }

        return _entities = entities;
      }
    }

    private State CreateStateFromTerm(PlTerm stateTerm)
    {
      var argsLst = PlTermExtension.ArgsLst(stateTerm);
      var type = new StateType(stateTerm.Name, stateTerm.Arity);

      if (!_StateTypeSpecs.ContainsKey(type))
      {
        return null;
      }
      var specsSet = _StateTypeSpecs[type];
      IEnumerable<StateTerm> stateTermsArgs = null;

      foreach (var specsLst in specsSet)
      {
        if (argsSatisfySpecs(specsLst, argsLst, out var stateTerms))
        {
          stateTermsArgs = stateTerms;
          break;
        }
      }
      return stateTermsArgs != null ? new State(type.Name, type.Arity, stateTermsArgs) : null;
    }

    private bool argsSatisfySpecs(IEnumerable<Type> types, IEnumerable<PlTerm> args, out ImmutableList<StateTerm> stateTerms)
    {
      stateTerms = ImmutableList.Create<StateTerm>();
      var typesArgs = types.Zip(args);
      foreach ((Type Type, PlTerm Arg) typeArg in typesArgs)
      {
        if (typeArg.Type == typeof(Entity))
        {
          if (typeArg.Arg.IsAtom && _Entities.Contains(typeArg.Arg.ToString()))
          {
            stateTerms = stateTerms.Add(new StateTerm(new Entity(typeArg.Arg.ToString())));
          }
          else
          {
            return false;
          }
        }
        else if (typeArg.Type == typeof(Scalar))
        {
          if (typeArg.Arg.IsNumber)
          {
            stateTerms = stateTerms.Add(new StateTerm(new Scalar(float.Parse(typeArg.Arg.ToString(), CultureInfo.InvariantCulture))));
          }
          else if (typeArg.Arg.IsAtom)
          {
            stateTerms = stateTerms.Add(new StateTerm(new Scalar(typeArg.Arg.ToString())));
          }
          else
          {
            return false;
          }
        }
        else if (typeArg.Type == typeof(State))
        {
          if (typeArg.Arg.IsCompound)
          {
            var internalState = CreateStateFromTerm(typeArg.Arg);
            if (internalState != null)
            {
              stateTerms = stateTerms.Add(new StateTerm(internalState));
            }
            else
            {
              return false;
            }
          }
          else
          {
            return false;
          }
        }
        else if (typeArg.Type == typeof(Event))
        {
          if (typeArg.Arg.IsCompound)
          {
            var internalEvent = CreateEventFromTerm(typeArg.Arg);
            if (internalEvent != null)
            {
              stateTerms = stateTerms.Add(new StateTerm(internalEvent));
            }
            else
            {
              return false;
            }
          }
          else
          {
            return false;
          }
        }
      }
      return true;
    }

    private ImmutableList<StateTerm> instanceStateTermArgs(IEnumerable<PlTerm> args)
    {
      var stateTerms = ImmutableList.Create<StateTerm>();
      foreach (var arg in args)
      {
        if (arg.IsAtom && _Entities.Contains(arg.ToString()))
        {
          stateTerms = stateTerms.Add(new StateTerm(new Entity(arg.ToString())));
        }
        else if (arg.IsAtom)
        {
          stateTerms = stateTerms.Add(new StateTerm(new Scalar(arg.ToString())));
        }
        else if (arg.IsNumber)
        {
          stateTerms = stateTerms.Add(new StateTerm(new Scalar(float.Parse(arg.ToString(), CultureInfo.InvariantCulture))));
        }
        else if (arg.IsCompound)
        {
          var internalState = CreateStateFromTerm(arg);
          if (internalState != null)
          {
            stateTerms = stateTerms.Add(new StateTerm(internalState));
          }
          else
          {
            var internalEvent = CreateEventFromTerm(arg);
            if (internalEvent != null)
            {
              stateTerms = stateTerms.Add(new StateTerm(internalEvent));
            }
          }
        }
        else
        {
          return null;
        }
      }
      return stateTerms;
    }

    private Event CreateEventFromTerm(PlTerm term)
    {
      if (term.Name != "event" || term.Arity != 2)
      {
        return null;
      }
      var queryEventArgs = new PlTerm[]
      {
        term[1],
        PlTerm.PlVar(),
        PlTerm.PlVar(),
        PlTerm.PlVar(),
        PlTerm.PlVar(),
        PlTerm.PlVar(),
        PlTerm.PlVar()
      };
      bool eventExists = PlQuery.PlCall("eventType", new PlTermV(queryEventArgs));
      if (!eventExists)
      {
        return null;
      }
      var argsLst = PlTermExtension.ArgsLst(term[1]);
      var stateTermsArgs =instanceStateTermArgs(argsLst);
      if (stateTermsArgs == null)
      {
        return null;
      }
      return new Event(term[1].Name, term[1].Arity, stateTermsArgs, float.Parse(term[2].ToString(), CultureInfo.InvariantCulture));
    }

    private PlTerm GetPlTermFromStateTerm(StateTerm stateTerm)
    {
      if (stateTerm.Type == typeof(Entity))
      {
        return new PlTerm(((Entity)stateTerm.Term).ToString());
      }
      if (stateTerm.Type == typeof(Scalar))
      {
        var scalar = (Scalar)stateTerm.Term;
        if (scalar.Type == typeof(float))
        {
          return new PlTerm((float)scalar.Value);
        }
        if (scalar.Type == typeof(string))
        {
          return new PlTerm((string)scalar.Value);
        }
      }
      if (stateTerm.Type == typeof(State))
      {
        return GetPlTermFromState((State)stateTerm.Term);
      }
      if (stateTerm.Type == typeof(Event))
      {
        return GetPlTermFromEvent((Event)stateTerm.Term);
      }
      return default;
    }

    private PlTerm GetPlTermFromState(State st)
    {
      var args = ImmutableArray.Create<PlTerm>();
      foreach (var arg in st.Args)
      {
        var argTerm = GetPlTermFromStateTerm(arg);
        args = args.Add(argTerm);
      }

      return PlTerm.PlCompound(st.Name, new PlTermV(args.ToArray()));
    }

    private PlTerm GetPlTermFromEvent(Event ev)
    {
      var args = ImmutableArray.Create<PlTerm>();
      foreach (var arg in ev.Args)
      {
        var argTerm = GetPlTermFromStateTerm(arg);
        args = args.Add(argTerm);
      }

      var eventSignature = PlTerm.PlCompound(ev.Name, new PlTermV(args.ToArray()));
      return PlTerm.PlCompound("event", eventSignature, new PlTerm(ev.OcurrenceTime));
    }

    public ImmutableHashSet<State> States
    {
      get
      {
        var statesSet = ImmutableHashSet.Create<State>();
        var stsColl = PlQuery.PlCallQuery("allStates(States)").AsEnumerable();
        foreach (var stateTerm in stsColl)
        {
          var state = CreateStateFromTerm(stateTerm);
          statesSet = statesSet.Add(state);
        }
        return statesSet;
      }
    }

    public ImmutableList<Event> Query(float currentTime, IEnumerable<Trigger> triggers)
    {
      var triggerTerms = triggers.Select(trigger => PlTerm.PlCompound(trigger.Name, new PlTerm(trigger.Time)));
      var triggersLst = PlTermExtension.PlList(triggerTerms);

      var queryArgs = new PlTerm[]
      {
        new PlTerm(currentTime),
        triggersLst,
        new PlTerm("TriggeredEvents")
      };
      var query = new PlQuery("query", new PlTermV(queryArgs));
      var triggeredEventsTerms = query.Solutions.First()[2].ToList();

      var triggeredEvents = ImmutableList.Create<Event>();

      foreach (var trgEvTerm in triggeredEventsTerms)
      {
        var triggeredEvent = CreateEventFromTerm(trgEvTerm);
        triggeredEvents = triggeredEvents.Add(triggeredEvent);
      }

      return triggeredEvents;
    }

    public void ExecuteEvent(string name, IEnumerable<StateTerm> args)
    {
      var evTerm = GetPlTermFromEvent(new Event(name, args.Count(), args, 0));
      PlQuery.PlCall("createAndExecuteEventNow", new PlTermV(evTerm[1]));
    }
  }
}
