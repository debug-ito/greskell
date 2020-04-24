package com.github.debug_ito.greskell;

import org.junit.Test;
import static org.junit.Assert.assertThat;
import static org.hamcrest.CoreMatchers.*;
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.__;
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.T;
import org.apache.tinkerpop.gremlin.process.traversal.Order;
import org.apache.tinkerpop.gremlin.process.traversal.P;
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
      fail("this operation is supposed to throw an exception");
    }catch(Exception e) {
      // expected.
      ;
    }
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
    // This is often found on the web,
    // e.g., https://stackoverflow.com/questions/51784430/why-do-you-need-to-fold-unfold-using-coalesce-for-a-conditional-insert
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
}
