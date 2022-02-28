import numpy as np
import networkx as nx
import csv


def load_network(network_name):
    net = nx.DiGraph()
    with open("../graphs/"+network_name+".graph", "r") as fh:
        el_reader = csv.reader(fh, delimiter=' ')
        el_header = next(el_reader)
        n_nodes = int(el_header[0])
        n_edges = int(el_header[1])
        net.add_nodes_from(range(0, n_nodes))
        for row in el_reader:
            net.add_edge(int(row[0]), int(row[1]))
    return net


def write_data(dm, data_name):
    file_name = '../data/' + data_name + '.matrix'
    with open(file_name, 'w', newline='') as text_file:
        text_file.write(str(dm.shape[0]) + ' ' + str(dm.shape[1]) + '\n')
    with open(file_name, 'a', newline='') as text_file:
        np.savetxt(text_file, dm, delimiter=' ')


def normalise(data):
    return (data - np.mean(data)) / np.std(data)


def generate(dag, n_points, rng, noise_level=0.05):
    ts = list(nx.algorithms.topological_sort(dag))
    for node in ts:
        if dag.in_degree(node) == 0:
            # print('Random', node)
            dag.nodes[node]['data'] = normalise(rng((n_points,)))
        else:
            parents = list(dag.predecessors(node))
            parents_sum = np.zeros((n_points,))
            for parent in parents:
                # print('  Adding', parent, dag.nodes[parent]['data'])
                parents_sum += dag.nodes[parent]['data']
            node_data_raw = parents_sum / dag.in_degree(node) + rng((n_points,)) * noise_level
            dag.nodes[node]['data'] = normalise(node_data_raw)
        # print('Data:', dag.nodes[node]['data'])

    data_matrix = np.empty((dag.number_of_nodes(), n_points))

    for (node_index, node) in enumerate(ts):
        data_matrix[node] = dag.nodes[node]['data']

    return data_matrix


def generate_data(net_name, nsmp=1000):
    graph_truth = load_network(net_name)
    dm = generate(graph_truth, nsmp, lambda s: np.random.uniform(-1, 1, s), 0.3)
    write_data(dm, net_name)


if __name__ == '__main__':
    np.random.seed(20170322)
    generate_data("alarm")
    generate_data("andes")
    generate_data("diabetes")
    generate_data("hailfinder")
    generate_data("hepar2")
    generate_data("link")
    generate_data("munin")
    generate_data("pathfinder")
    generate_data("pigs")
    generate_data("win95pts")
    # generate_data("random1kf2")
    # generate_data("random1kf3")
    # generate_data("random1kf4")
    generate_data("random5kf2")
    generate_data("random5kf3")
    generate_data("random5kf4")
