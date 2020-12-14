package com.github.debug_ito.greskell;

import org.junit.Test;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.hamcrest.CoreMatchers.*;
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.__;
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.T;
import org.apache.tinkerpop.gremlin.process.traversal.Order;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.process.traversal.TextP;
import org.apache.tinkerpop.gremlin.process.traversal.Path;
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversalSource;

public class TestGremlin {
  @Test
  public void filter_resets_traversal() throws Exception {
    def g = MyModern.make().traversal();
    def without_filter = g.V().has("name", "marko").path().toList();
    def with_filter = g.V().has("name", "marko").filter(__.out().out()).path().toList();
    assertThat with_filter, is(without_filter);
  }

  @Test
  public void aggregate_sideEffect_affects_parents() throws Exception {
    def g = MyModern.make().traversal();
    def got_se = g.V().sideEffect(__.sideEffect(__.sideEffect(__.aggregate("x")))).cap("x").next();
    assertThat got_se.size(), is(6);

    def got_fil = g.V().filter(__.filter(__.filter(__.aggregate("x")))).cap("x").next();
    assertThat got_fil.size(), is(6);

    def got_loc = g.V().local(__.local(__.local(__.aggregate("x")))).cap("x").next();
    assertThat got_loc.size(), is(6);
  }

  @Test
  public void GTraversal_is_not_Category() throws Exception {
    def g = MyModern.make().traversal();
    def a = { g.V() };
    def b = { __.identity() };
    def ab = a().repeat(b()).times(1).toList();
    def ba = b().repeat(a()).times(1).toList();
    assertThat ab, is(not(ba));
  }

  @Test
  public void as_is_cancalled_in_splitting_traversal() throws Exception {
    def g = MyModern.make().traversal();
    assertThat g.V().as("x").out().as("y").select("x").toList().size(), is(6);
    assertThat g.V().as("x").out().as("y").select("y").toList().size(), is(6);
    assertThat g.V().sideEffect(__.as("x").out().as("y")).select("x").toList().size(), is(0);
    assertThat g.V().sideEffect(__.as("x").out().as("y")).select("y").toList().size(), is(0);
    assertThat g.V().filter(__.as("x").out().as("y")).select("x").toList().size(), is(0);
    assertThat g.V().filter(__.as("x").out().as("y")).select("y").toList().size(), is(0);
    
    assertThat g.V().map(__.as("x").out().as("y")).select("x").toList().size(), is(0);
    assertThat g.V().flatMap(__.as("x").out().as("y")).select("x").toList().size(), is(0);
  }

  @Test
  public void partial_null_in_select() throws Exception {
    def g = MyModern.make().traversal();
    def got = g.V().has("name", "marko").as("person").out().as("out").values("age").as("age").select("person", "out", "age").toList();
    assertThat got.collect { it.get("person").value("name") }, is(["marko", "marko"]);
    assertThat got.collect { it.get("out").label() }, is(["person", "person"]);
    assertThat got.collect { it.get("age") }.sort(), is([27, 32]);
    
    // "out" label points to three nodes (josh(person), vadas(person),
    // lop(software)), but only "person" nodes have "age" property. I
    // think .select step emits traversers in which all selected
    // labels have at least one element.
  }

  @Test
  public void flatMap_map_affect_path() throws Exception {
    def g = MyModern.make().traversal();
    assertThat g.V().map(__.identity()).path().next().size(), is(2);
    assertThat g.V().flatMap(__.identity()).path().next().size(), is(2);
  }

  @Test
  public void flatMap_map_cancel_child_travasal_paths() throws Exception {
    def g = MyModern.make().traversal();
    assertThat g.V().out().in().out().in().path().next().size(), is(5);
    assertThat g.V().map(__.out().in().out().in()).path().next().size(), is(2);
    assertThat g.V().flatMap(__.out().in().out().in()).path().next().size(), is(2);
  }

  @Test
  public void order_by_projection_traversal_uses_the_first_element() throws Exception {
    def graph = TinkerGraph.open();
    def g = graph.traversal();
    g.addV("target").property("name","A")
    .addV("target").property("name","B")
    .addV("valA").property("v",10)
    .addV("valA").property("v",20)
    .addV("valA").property("v",30)
    .addV("valB").property("v",5)
    .addV("valB").property("v",15)
    .addV("valB").property("v",25)
    .addV("valB").property("v",35)
    .addV("valB").property("v",45).iterate();
    g.V().hasLabel("target").has("name","A").as("target_A").V().hasLabel("valA").addE("has_value").from("target_A").iterate();
    g.V().hasLabel("target").has("name","B").as("target_B").V().hasLabel("valB").addE("has_value").from("target_B").iterate();
    assertThat g.V().hasLabel("target").has("name","A").out().values("v").order().by(Order.incr).toList(), is([10,20,30]);
    assertThat g.V().hasLabel("target").has("name","B").out().values("v").order().by(Order.incr).toList(), is([5,15,25,35,45]);

    assertThat g.V().hasLabel("target").order().by(__.out().values("v").order().by(Order.incr), Order.incr).values("name").toList(), is(["B", "A"]);
    assertThat g.V().hasLabel("target").order().by(__.out().values("v").order().by(Order.decr), Order.incr).values("name").toList(), is(["A", "B"]);
  }

  @Test
  public void path_by_projection_traversal_uses_the_first_element() throws Exception {
    def g = MyModern.make().traversal();
    assertThat g.V().has("name", "marko").path().by("name")
               .map({ it.get().objects() }).toList(), is([["marko"]]);
    assertThat g.V().has("name", "marko").path().by(__.outE("knows").order().by("weight",Order.decr).inV().values("name"))
               .map({ it.get().objects() }).toList(), is([["josh"]]);
  }

  @Test
  public void addE_traversal_takes_input_to_addE() throws Exception {
    def g = MyModern.make().traversal();
    def edges = g.V().has("name", P.within("marko", "peter", "josh"))
                .addE("new_edge").from(__.outE("created").order().by("weight", Order.incr).inV()).toList();
    // The traversal inside .from() yields 2 anchor vertices for
    // "josh" (namely, "lop" and "ripple"), but only the first vertex
    // is used for the anchor. As a result, .addE() always creates
    // exactly one edge for each input Vertex.
    assertThat edges.size(), is(3);
    edges.each { e ->
      assertThat((e instanceof Edge), is(true));
      assertThat(e.label(), is("new_edge"));
    };
    def pairs = edges.collect { e -> return (String)(e.outVertex().value("name")) + "->" + (String)(e.inVertex().value("name")) };
    assertThat pairs.sort(), is(["lop->josh", "lop->marko", "lop->peter"]);
    assertThat(g.E().hasLabel("new_edge").toList().size(), is(3));
  }

  @Test
  public void addE_traversal_throws_error_if_it_yields_no_result() throws Exception {
    def g = MyModern.make().traversal();
    try {
      g.V().has("name", "vadas").addE("new_edge").from(__.out("created")).iterate();
    }catch(Exception e) {
      // expected.
      ;
      return;
    }
    fail("this operation is supposed to throw an exception");
  }

  private static String pathToString(Path p) {
    return p.objects().collect { elem ->
      if(elem instanceof Vertex) {
        return "v(" + (String)(((Vertex)elem).value("name")) + ")";
      }else if(elem instanceof Edge) {
        def e = (Edge)elem;
        return "e(" + (String)e.outVertex().value("name") + "-" + e.label() + "->" +
          (String)e.inVertex().value("name") + ")";
      }
    }.join(",");
  }

  @Test
  public void V_method_flatMaps_the_input_traverser() throws Exception {
    def g = MyModern.make().traversal();
    def paths = g.E().hasLabel("created").outV().as("creator").V().has("name", P.within("vadas", "ripple")).path().toList();
    def paths_str = paths.collect { p -> return pathToString(p); };
    assertThat paths_str.sort(), is([
      "e(josh-created->lop),v(josh),v(ripple)",
      "e(josh-created->lop),v(josh),v(vadas)",
      "e(josh-created->ripple),v(josh),v(ripple)",
      "e(josh-created->ripple),v(josh),v(vadas)",
      "e(marko-created->lop),v(marko),v(ripple)",
      "e(marko-created->lop),v(marko),v(vadas)",
      "e(peter-created->lop),v(peter),v(ripple)",
      "e(peter-created->lop),v(peter),v(vadas)",
     ]);
  }

  @Test
  public void repeat_step_is_transparent_about_internal_traversal() throws Exception {
    def g = MyModern.make().traversal();
    def paths_str =
      g.V().has("name", "marko").repeat(__.identity()).times(2).path().toList().collect { p -> return pathToString(p); };
    assertThat paths_str, is(["v(marko)"]);
  }

  @Test
  public void repeat_step_without_until_or_times() throws Exception {
    def g = MyModern.make().traversal();
    def paths_str =
      g.V().has("name", "marko").repeat(__.out()).path().toList().collect { p -> return pathToString(p); };
    assertThat paths_str, is([]);
  }

  @Test
  public void repeat_step_times() throws Exception {
    def g = MyModern.make().traversal();
    def paths_str_first0 = g.V().has("name", "marko").times(0).repeat(__.out()).path().toList().collect { p -> return pathToString(p); };
    def paths_str_first1 = g.V().has("name", "marko").times(1).repeat(__.out()).path().toList().collect { p -> return pathToString(p); };
    def paths_str_first2 = g.V().has("name", "marko").times(2).repeat(__.out()).path().toList().collect { p -> return pathToString(p); };
    def paths_str_last0 = g.V().has("name", "marko").repeat(__.out()).times(0).path().toList().collect { p -> return pathToString(p); };
    def paths_str_last1 = g.V().has("name", "marko").repeat(__.out()).times(1).path().toList().collect { p -> return pathToString(p); };
    def paths_str_last2 = g.V().has("name", "marko").repeat(__.out()).times(2).path().toList().collect { p -> return pathToString(p); };
    assertThat paths_str_first0.sort(), is(["v(marko)"]);
    assertThat paths_str_first1.sort(), is(["v(marko),v(josh)", "v(marko),v(lop)", "v(marko),v(vadas)"]);
    assertThat paths_str_first2.sort(), is([
      "v(marko),v(josh),v(lop)",
      "v(marko),v(josh),v(ripple)",
    ]);
    assertThat paths_str_last0.sort(), is(["v(marko),v(josh)", "v(marko),v(lop)", "v(marko),v(vadas)"]);
    assertThat paths_str_last1.sort(), is(["v(marko),v(josh)", "v(marko),v(lop)", "v(marko),v(vadas)"]);
    assertThat paths_str_last2.sort(), is([
      "v(marko),v(josh),v(lop)",
      "v(marko),v(josh),v(ripple)",
    ]);
  }

  @Test
  public void repeat_step_outputs_both_emit_and_final_results() throws Exception {
    def got = __.__(1).repeat(__.map { it.get() * 2 }).emit { it.get() < 10 }.times(5).toList();
    assertThat got, is([
        2, 4, 8,  // emit result
        32        // final result
      ]);
  }

  @Test
  public void repeat_step_dedup_emit_and_final_results() throws Exception {
    def got = __.__(1).repeat(__.map { it.get() * 2 }).emit().times(5).toList();
    assertThat got, is([
      2, 4, 8, 16, 32 // the final result is output by emit. It's not output as the final result again.
    ]);
  }

  @Test
  public void union_step_without_arg() throws Exception {
    def g = MyModern.make().traversal();
    def paths_str = g.V().has("name", "marko").union().path().toList().collect { p -> return pathToString(p); };
    assertThat paths_str, is([]);
  }

  @Test
  public void union_step_with_identity() throws Exception {
    def g = MyModern.make().traversal();
    def paths_str = g.V().has("name", "marko")
      .union(__.identity(), __.identity()).path().toList().collect { p -> return pathToString(p); };
    assertThat paths_str, is(["v(marko)", "v(marko)"]);
  }

  @Test
  public void union_step_with_transformation() throws Exception {
    def g = MyModern.make().traversal();
    def paths_str = g.V().has("name", "marko")
      .union(
        __.outE("knows").inV().order().by("age"),
        __.outE("created").inV()
      ).path().toList().collect { p -> return pathToString(p); };
    assertThat paths_str, is([
      "v(marko),e(marko-knows->vadas),v(vadas)",
      "v(marko),e(marko-knows->josh),v(josh)",
      "v(marko),e(marko-created->lop),v(lop)"
    ]);
  }

  @Test
  public void choose_step_transform_in_predicate() throws Exception {
    def g = MyModern.make().traversal();
    def pathsWithAge = { age_thresh ->
      return g.V().hasLabel("person").choose(
        __.out("knows").has("name", "josh").has("age", P.gt(age_thresh)),
        __.identity(),
        __.not(__.identity()),
      ).path().toList().collect { p -> return pathToString(p); };
    };
    assertThat pathsWithAge(30), is(["v(marko)"]);
    assertThat pathsWithAge(40), is([]);
  }

  @Test
  public void choose_step_transform_in_result() throws Exception {
    def g = MyModern.make().traversal();
    def pathsWithAge = { age_thresh ->
      return g.V().has("name", "marko").choose(
        __.has("age", P.gt(age_thresh)),
        __.out("created"),
        __.out("knows")
      ).path().toList().collect { p -> return pathToString(p); };
    };
    assertThat pathsWithAge(25).sort(), is(["v(marko),v(lop)"]);
    assertThat pathsWithAge(30).sort(), is(["v(marko),v(josh)", "v(marko),v(vadas)"]);
  }

  @Test
  public void constant_step_and_path() throws Exception {
    def path_objs = __.__(1,2,3).constant(999).path().toList().collect { p -> p.objects(); };
    assertThat path_objs, is([[1,999], [2,999], [3,999]]);
  }

  @Test
  public void constant_step_to_void() throws Exception {
    def got = __.__(1,2,3).not(__.identity()).constant(999).toList();
    assertThat got, is([]);
  }

  @Test
  public void dedup_step_depends_on_order() throws Exception {
    def g = MyModern.make().traversal();

    // josh (person)
    // lop (software)
    // marko (person)
    // peter (person)
    // ripple (software)
    // vadas (person)

    // Note: we need barrier() step before dedup() to ensure the order
    // of the input to dedup.
    
    assertThat(
      g.V().order().by("name", Order.incr).barrier().dedup().by(T.label).values("name").toList(),
      is(["josh", "lop"])
    );
    assertThat(
      g.V().order().by("name", Order.decr).barrier().dedup().by(T.label).values("name").toList(),
      is(["vadas", "ripple"])
    );
  }

  @Test
  public void addV_step_add_vertex_for_each_input() throws Exception {
    def graph = TinkerGraph.open();
    def g = graph.traversal();
    assertThat g.V().count().next(), is(0L);
    g.inject(1,2,3).addV("foobar").iterate();
    assertThat g.V().count().next(), is(3L);
  }

  static private void getOrAdd_with_fold(GraphTraversalSource g, String name) throws Exception {
    // This is often found on the web, e.g.,
    //
    // - https://stackoverflow.com/questions/51784430/why-do-you-need-to-fold-unfold-using-coalesce-for-a-conditional-insert
    // - https://stackoverflow.com/questions/46027444/gremlin-only-add-a-vertex-if-it-doesnt-exist
    g.V().has("name", name).fold().coalesce(
      __.unfold(),
      __.addV("person").property("name", name)
    ).iterate();
  }
  
  static private void getOrAdd_with_inject(GraphTraversalSource g, String name) throws Exception {
    // This is relatively rare, but you can find it on the web.
    //
    // - https://stackoverflow.com/questions/56207963/the-gremlin-coalesce-step-is-inconsistent-cosmos-db-in-general
    // - https://stackoverflow.com/questions/52447308/add-edge-if-not-exist-using-gremlin
    // - https://stackoverflow.com/questions/41314091/how-to-add-a-vertex-only-if-it-doesnt-exist-and-continue-this-single-traversal
    g.inject(1).coalesce(
      __.V().has("name", name),
      __.addV("person").property("name", name)
    ).iterate();
  }

  static private void getOrAdd(GraphTraversalSource g, String name) throws Exception {
    getOrAdd_with_fold(g, name);
  }

  @Test
  public void getOrAdd_should_get_or_add_vertex() throws Exception {
    def graph = TinkerGraph.open();
    def g = graph.traversal();
    def getNames = {
      return g.V().values("name").order().by(__.identity()).toList();
    };
    assertThat getNames(), is([]);
    getOrAdd(g, "foo");
    assertThat getNames(), is(["foo"]);
    getOrAdd(g, "foo");
    assertThat getNames(), is(["foo"]);
    getOrAdd(g, "bar");
    assertThat getNames(), is(["bar", "foo"]);
    getOrAdd(g, "foo");
    assertThat getNames(), is(["bar", "foo"]);
  }

  @Test
  public void coalesce_step_adds_a_path_step() throws Exception {
    def got = __.__(1,2,3).coalesce(
      __.is(P.gte(3)),
      __.identity()
    ).path().toList().collect { p -> p.objects(); };
    assertThat got, is([[1, 1], [2, 2], [3, 3]]);
  }

  @Test
  public void coalesce_step_with_empty_traversal() throws Exception {
    def got = __.__(1,2,3).coalesce().toList();
    assertThat got, is([]);
  }

  @Test
  public void coalesce_step_clear_path_history() throws Exception {
    def got = __.__(1,2,3).coalesce(
      __.is(P.gte(3)).map { it.get() * 2 }.map { it.get() + 10 }.map { it.get() + 20 },
      __.map { it.get() * 10 }.map { it.get() + 8 },
    ).path().toList().collect { p -> p.objects(); };
    assertThat got, is([
      [1, 18],
      [2, 28],
      [3, 36]
    ]);
  }

  @Test
  public void iterate_method_returns_empty_traversal() throws Exception {
    def got = __.__(1,2,3).iterate();
    assertThat got.hasNext(), is(false);
  }

  @Test
  public void match_step_output() throws Exception {
    def got = __.__(1,2,3,4).as("a").match(
      __.or(
        __.as("b").map { it.get() * 2 }.as("c"),
        __.as("b").map { it.get() + 5 }.as("c"),
      ),
      __.as("b").map { it.get() + 2 }.as("d"),
      __.as("c").is(6)
    ).path().toList();
    
    assertThat got.size(), is(2);

    // In practice, the traverser value emitted from match() step is
    // undefined. That's because it's so subject to query
    // optimization. See the discussion in the following threads.
    //
    // - https://groups.google.com/g/gremlin-users/c/CD0EQNm7U3c/
    // - https://groups.google.com/g/gremlin-users/c/HVtldzV0Xk8
    //
    //
    // def got_results = got.collect { p ->
    //   def objs = p.objects();
    //   def match_result = objs[objs.size() - 1];
    //   return match_result;
    // }.toSet();
    // assertThat got_results, is([["b": 1, "c": 6, "d": 3], ["b": 3, "c": 6, "d": 5]] as Set);

    
    // A traverser emitted from the match step has path history that
    // includes all matched patterns. The path history includes `as`
    // labels assigned both inside and outside of `match` step. This
    // is explicitly documented in TinkerPop reference manual in an
    // "IMPORTANT" column.
    // 
    // However, the order of matched patterns that appear in the path
    // history is not deterministic.

    def got_history = got.collect { p ->
      def his = [:];
      ["a", "b", "c", "d"].each { label ->
        his[label] = p.get(label);
      };
      return his;
    }.toSet();
    assertThat got_history, is([["a":1, "b": 1, "c": 6, "d": 3], ["a":3, "b":3, "c":6, "d":5]] as Set)
  }

  @Test
  public void match_without_pattern() throws Exception {
    try {
      def got = __.__(1,2,3,4).match().path().toList();
    } catch(Exception e) {
      ; // expected.
      return;
    }
    fail("match step without pattern should throw exception.");
  }

  @Test
  public void match_referring_to_history_label() throws Exception {
    def got = __.__(1,2,3,4).as("a").map { it.get() * 2 }.as("b").match(
      __.as("a").or(__.is(1),  __.is(4)),     // the start label refers to a history label
      __.as("b").identity().as("c"),          // the end label makes a new binding
      __.as("c").map { it.get() - 1 }.as("a") // the end label refers to a history label
    ).select("a", "b", "c").toList(); // We should select() the result labels explicitly.
    assertThat got, is([["a": 1, "b": 2, "c": 2]]); // the result includes history labels the match refers to.
  }

  @Test
  public void match_nested() throws Exception {
    def got = __.__(1,2,3,4).as("H").map{ it.get() * 2 }.match(
      __.as("O1").map { it.get() + 3 }.match(
        __.as("H").identity().as("I1"), // Inner match refers to history label
        __.as("I1").map { it.get() + 9 }.as("O3"), // Inner match refers to a binding in outer match
      ).select("H", "I1", "O3").as("O2"), // We should explicitly select()
      __.as("O1").map { it.get() * 2 }.as("O3"),

      __.as("I1").identity().as("O4") // Outer match refers to a binding in inner match (is it just referring to path history?)
    ).select("O1", "O2", "O3", "O4", "I1").toList(); // We should explicitly select()
    
    // The result of a match includes labels appeared in patterns (regardless of those labels are new bindings or not)
    assertThat got, is([["O1":6, "O2":["H":3, "I1":3, "O3":12], "O3":12, "O4":3, "I1":3]]);
  }

  @Test
  public void match_multiple_free_start_variables() throws Exception {
    try {
      __.__(1,2,3,4).match(
        __.as("a").map { it.get() * 2 }.as("b"),
        __.as("c").map { it.get() * 4 }.as("d")
      ).iterate();
    }catch(Exception e) {
      return;
    }
    fail("This operation is supposed to throw an exception");
  }

  @Test
  public void match_start_labels_free_and_history() throws Exception {
    try {
      __.__(1,2,3,4).as("a").map { it.get() * 2 }.match(
        __.as("a").map { it.get() * 4 }.as("b"),  // start label refers to a history label
        __.as("c").map { it.get() * 2 }.as("d"),  // start label as a free variable
      ).iterate();
    }catch(Exception e) {
      return;
    }
    fail("This operation is supposed to throw an exception.");
  }

  //// I think the start label is either of the following three cases:
  //// 
  //// 1. Internally bound variable: a label that is also an end label of other
  ////    patterns.
  //// 2. History variable: a label that is already contained in the path history of
  ////    the traverser when it enters the match step.
  //// 3. Free variable: None of the above.
  //// 
  //// Arguments of match step can include as many patterns with type 1.
  //// 
  //// It seems that patterns of type 2 and 3 are exclusive. That is, if the arguments
  //// contain a pattern of type 2, they cannot also include a pattern of type 3.
  //// Multiple patterns of type 2 can have different start labels. On the other hand,
  //// all patterns of type 1 must have the same start label.
  ////
  //// Maybe the above constraint is a bug, and will be (or has already been) fixed.
  ////
  //// Maybe https://issues.apache.org/jira/browse/TINKERPOP-2230 fixed it??

  @Test
  public void match_pattern_without_start_label() throws Exception {
    try {
      __.__(1,2,3,4).match(
        __.map { it.get() * 3 }.as("b")
      ).iterate();
    }catch (Exception e) {
      return;
    }
    fail("this operation is supposed to throw an exception");
  }

  @Test
  public void match_where_with_no_label() throws Exception {
    try {
      __.__(1,2,3,4).match(
        __.as("a").map { it.get() * 3 }.as("b"),
        __.where( __.map { it.get() + 1 }.is(10) )
      ).iterate();
    }catch(Exception e) {
      // expected
      return;
    }
    fail("This operation is supposed to throw an exception");
  }

  // It's practically undefined how it behaves when there is no start
  // label in a match pattern.
  //
  // @Test
  // public void match_where_with_no_start_label() throws Exception {
  //   def got = __.__(1,2,3,4).match(
  //     __.as("a").map { it.get() * 3 }.as("b"),
  //     __.where( __.map { it.get()  }.as("a") )
  //   ).toList();
  // }
  // 
  // @Test
  // public void match_whereP_with_no_start_label() throws Exception {
  //   def got = __.__(1,2,3,4).match(
  //     __.as("a").map { it.get() * 3 }.as("b"),
  //     __.where(P.eq("b"))
  //   ).toList();
  // }

  @Test
  public void match_with_or_exceptions() throws Exception {
    Exception got;
    
    // "where" without start label inside "or". Exception from this
    // pattern is reasonable, because traversals in "or" should be
    // able to run in parallel.
    try {
      __.__(1,2,3,4).match(
        __.or(
          __.as("a").map { it.get() * 3 }.as("b"),
          __.where(__.map { it.get() + 6 }.as("a"))
        )
      ).iterate();
    }catch (Exception e) {
      got = e;
    }
    assertThat got, is(not(null));

    // "or" with single traversal. I don't understand why this emits
    // an exception. Maybe this is a bug.
    got = null;
    try {
      __.__(1,2,3,4).match(
        __.or(
          __.as("a").map { it.get() * 3 }.as("b"),
        )
      ).iterate();
    }catch (Exception e) {
      got = e;
    }
    assertThat got, is(not(null));
    
    // "where" in "or", but the "where" only refers to the label
    // defined in the outer match pattern with "and" relationship. I
    // think this is reasonable, but it emits an exception.
    got = null;
    try {
      __.__(1,2,3,4).match(
        __.as("c").map { it.get() * 3 }.as("d"),
        __.or(
          __.as("a").map { it.get() * 3 }.as("b"),
          __.where(__.map { it.get() + 6 }.as("d")),
        )
      ).iterate();
    }catch (Exception e) {
      got = e;
    }
    assertThat got, is(not(null));
  }

  @Test
  public void where_with_TextP() throws Exception {
    def got = __.__("abc", "def", "ghi").as("A").map { it.get() + "ghi" }.where(TextP.endingWith("A")).toList();
    assertThat got, is(["ghighi"]);
  }

  @Test
  public void where_with_TextP_and_by() throws Exception {
    def got = __.__(1,2,3).as("A").map { it.get() + 1 }.where(TextP.endingWith("A")).by(__.map {
        switch(it.get()) {
          case 1:
            return "aaa";
            break;
          case 2:
            return "bbb";
            break;
          case 3:
            return "ccc";
            break;
          case 4:
            return "bbbccc";
        }
      }).toList();
    assertThat got, is([4]);
  }

  @Test
  public void where_with_TestP() throws Exception {
    try {
      __.__(1,2,3).as("A").map { it.get() * 2 }.where(TextP.endingWith("A")).iterate();
    }catch (Exception e) {
      return;
    }
    fail("this operation is supposed to throw an exception, because it tries to cast the Integer input to String.");
  }
}
