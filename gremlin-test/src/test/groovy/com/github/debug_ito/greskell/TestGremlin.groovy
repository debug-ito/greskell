package com.github.debug_ito.greskell;

import org.junit.Test;
import static org.junit.Assert.assertThat;
import static org.hamcrest.CoreMatchers.*;
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.__;
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;
import org.apache.tinkerpop.gremlin.process.traversal.Order;


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
}
