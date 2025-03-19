// @ts-check

/**
 * @typedef {Object} TraceGraphSnapshot
 * @property {Record<string, number>} roots
 * @property {TraceNode[]} nodes
 * @property {TraceGraphPath} path
 */

/**
 * @typedef {Object} TraceNode
 * @property {number} id
 * @property {string} resolvedId
 * @property {Record<string, number>} children
 */

/**
 * @typedef {Object} TraceGraphPath
 * @property {string} specifier
 * @property {number} nodeId
 * @property {string} nv
 * @property {?TraceGraphPath} previous
 */

// @ts-types="npm:@types/d3@7.4"
import * as d3 from "https://cdn.jsdelivr.net/npm/d3@7/+esm";

/**
 * @type {TraceGraphSnapshot[]}
 */
// @ts-ignore rawTraces is defined in the generated html file.
const traces = rawTraces;
/** @type {any} */
let transform;
let currentIndex = 0;
const graphDiv = document.getElementById("graph");
let graph = createGraph(traces[0], onNodeSelect);
initSlider(traces.length, (index) => {
  currentIndex = index;
  const svg = d3.select("#graph");
  if (!svg.empty()) {
    // capture current zoom
    transform = d3.zoomTransform(svg.node());
  }

  graphDiv.replaceChildren(); // remove the children
  graph = createGraph(traces[index], onNodeSelect);
});

/** @param {number} id */
function onNodeSelect(id) {
}

/**
 * @param {number} max
 * @param {(value: number) => void} onChange */
function initSlider(max, onChange) {
  /** @type {HTMLInputElement} */
  const input = document.querySelector("#slider input");
  input.min = "0";
  input.max = max.toString();
  input.addEventListener("input", () => {
    // todo: debounce
    onChange(input.valueAsNumber);
  });
  input.value = "0";
}

/**
 * @param {TraceGraphSnapshot} snapshot
 * @param {(id: number) => void} onNodeSelect
 */
function createGraph(snapshot, onNodeSelect) {
  /** @param {d3.Selection<SVGSVGElement, any, HTMLElement, any>} svg  */
  function zoom(svg) {
    svg.call(
      d3
        .zoom()
        .scaleExtent([0.5, 5]) // Min and max zoom levels
        .on("zoom", (event) => g.attr("transform", event.transform)) // Apply zoom
    );
  }

  const { links, nodes } = getNodesAndLinks(snapshot);

  let wasMouseActivity = false;
  const width = graphDiv.clientWidth;
  const height = graphDiv.clientHeight;
  const svg = d3
    .select("#graph")
    .append("svg")
    .style("font", "40px sans-serif")
    .attr("width", width)
    .attr("height", height)
    .on("wheel", () => wasMouseActivity = true)
    .on("click", () => wasMouseActivity = true)
    .call(zoom);
  const simulation = d3.forceSimulation(nodes)
    .force(
      "link",
      d3.forceLink(links).id(/** @param {any} d */ (d) => d.id).distance(50),
    )
    .force("charge", d3.forceManyBody().strength(-300))
    .force("y", d3.forceCenter(width / 2, height / 2))
    .force("collide", d3.forceCollide(30));

  const g = svg.append("g");
  if (transform != null) {
    g.attr("transform", transform);
  }
  const link = g
    .selectAll(".link")
    .data(links)
    .enter()
    .append("line")
    .attr("stroke", "black")
    .attr("stroke-width", 1.5);

  const node = g
    .selectAll(".node")
    .data(nodes)
    .enter()
    .append("circle")
    .attr("r", 8)
    .attr("fill", "blue")
    .call(drag(simulation));

  const label = g
    .selectAll(".label")
    .data(nodes)
    .enter()
    .append("text")
    .text((d) => d.rawNode.resolvedId)
    .attr("font-size", "12px")
    .attr("dx", 10)
    .attr("dy", 4);

  simulation.on("tick", () => {
    link
      .attr("x1", (d) => d.source.x)
      .attr("y1", (d) => d.source.y)
      .attr("x2", (d) => d.target.x)
      .attr("y2", (d) => d.target.y);

    node.attr("cx", (d) => d.x).attr("cy", (d) => d.y);

    label.attr("x", (d) => d.x).attr("y", (d) => d.y);
  });

  function drag(simulation) {
    return d3
      .drag()
      .on("start", (event, d) => {
        if (!event.active) simulation.alphaTarget(0.3).restart();
        d.fx = d.x;
        d.fy = d.y;
      })
      .on("drag", (event, d) => {
        d.fx = event.x;
        d.fy = event.y;
      })
      .on("end", (event, d) => {
        if (!event.active) simulation.alphaTarget(0);
        d.fx = null;
        d.fy = null;
      });
  }

  let lastId = 0;
  return {
    /** @param {number} selectedNodeId */
    setSelectedNodeId(selectedNodeId) {
    },
  };
}

/**
 * @typedef {Object} GraphNode
 * @property {TraceNode} rawNode
 * @property {number} id
 * @property {GraphNode[]} sources
 * @property {GraphNode[]} targets
 * @property {number} depthY
 */

/** @param {TraceGraphSnapshot} snapshot */
function getNodesAndLinks(snapshot) {
  /** @type {GraphNode[]} */
  const nodes = snapshot.nodes.map((node) => ({
    id: node.id,
    rawNode: node,
    sources: /** @type {GraphNode[]} */ ([]),
    targets: /** @type {GraphNode[]} */ ([]),
    depthY: 0,
  }));
  const nodesMap = new Map(nodes.map((n) => [n.rawNode.id, n]));
  /** @type {{ source: number; target: number; color: string | undefined; originatingNodeId: number | undefined }[]} */
  const links = [];

  for (const node of nodes) {
    const rawNode = node.rawNode;
    for (const [specifier, child] of Object.entries(rawNode.children)) {
      addLink(node, getNodeById(child));
    }
  }

  /** @type {Set<number>} */
  const analyzedNodes = new Set();
  for (const start of Object.values(snapshot.roots)) {
    setDepthY(getNodeById(start));
  }

  return { nodes, links };

  /** @param {GraphNode} firstNode */
  function setDepthY(firstNode) {
    const nodesToAnalyze = [firstNode];

    while (nodesToAnalyze.length > 0) {
      const node = nodesToAnalyze.pop();
      if (node == null) {
        continue;
      }
      node.depthY = node.sources.length === 0
        ? 0
        : Math.max(...node.sources.map((s) => s.depthY)) + 1;
      if (!analyzedNodes.has(node.rawNode.id)) {
        analyzedNodes.add(node.rawNode.id);
        nodesToAnalyze.push(...node.targets);
      }
    }
  }

  /**
   * @param {GraphNode} source
   * @param {GraphNode} target
   * @param {string} [color]
   * @param {number} [originatingNodeId]
   */
  function addLink(source, target, color, originatingNodeId) {
    source.targets.push(target);
    target.sources.push(source);
    links.push({
      source: source.id,
      target: target.id,
      color,
      originatingNodeId,
    });
  }

  /** @param {number} id */
  function getNodeById(id) {
    const node = nodesMap.get(id);
    if (node == null) {
      throw new Error(`Could not find node: ${id}`);
    }
    return node;
  }
}
