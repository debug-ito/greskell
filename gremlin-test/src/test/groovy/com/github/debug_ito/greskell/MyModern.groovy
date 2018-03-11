package com.github.debug_ito.greskell;

import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.Edge;

public class MyModern {
  public Graph graph;

  /**
   * Create the "modern" toy graph of TinkerPop-3.3.0
   * documentation. We don't use TinkerFactory.createModern(), because
   * it may be unstable across TinkerPop versions.
   */
  static public Graph make() {
    MyModern m = new MyModern();
    m.init();
    return m.graph;
  }

  private MyModern() {
    graph = null;
  }
  
  private void init() {
    graph = TinkerGraph.open();
    Vertex marko = makePerson("marko", 29);
    Vertex vadas = makePerson("vadas", 27);
    Vertex lop = makeSoftware("lop", "java");
    Vertex josh = makePerson("josh", 32);
    Vertex ripple = makeSoftware("ripple", "java");
    Vertex peter = makePerson("peter", 35)
    
    makeEdge(marko, vadas, "knows", 0.5);
    makeEdge(marko, josh, "knows", 1.0);
    makeEdge(marko, lop, "created", 0.4);
    makeEdge(josh, ripple, "created", 1.0);
    makeEdge(josh, lop, "created", 0.4);
    makeEdge(peter, lop, "created", 0.2);
  }

  public Vertex makePerson(String name, int age) {
    Vertex v = graph.addVertex("person");
    v.property("name", name);
    v.property("age", age);
    return v;
  }

  public Vertex makeSoftware(String name, String lang) {
    Vertex v = graph.addVertex("software");
    v.property("name", name);
    v.property("lang", lang);
    return v;
  }

  public Edge makeEdge(Vertex from, Vertex to, String label, double weight) {
    return from.addEdge(label, to, "weight", weight);
  }

}
