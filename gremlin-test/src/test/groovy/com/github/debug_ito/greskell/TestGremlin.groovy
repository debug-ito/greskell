package com.github.debug_ito.greskell;

import org.junit.Test;
import static org.junit.Assert.assertThat;
import static org.hamcrest.CoreMatchers.*;
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.__;


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

}
