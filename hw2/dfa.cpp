#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <queue>

typedef std::vector<std::vector<int > > vvi;
typedef std::vector<int> vi;

struct automaton {
	automaton() {};
	~automaton() {};

	void read(std::istream &in) {
		int m;
		in >> sigma;
		in >> n;
		in >> s >> n_term;
		s--;
		t.resize(n);
		for (std::size_t i = 0; i < n_term; i++) {
			int q;
			in >> q;
			t[q - 1] = 1;
		}
		g.resize(n, vi(sigma, -1));
		g_rev.resize(n, vvi(sigma));
		for (std::size_t i = 0; i < n; i++) {
			for (std::size_t j = 0; j < sigma; j++) {
				int v;
				in >> v;
				g[i][j] = v - 1;
				g_rev[v - 1][j].push_back(i);
			}
		}
	}

	void print(std::ostream &out) {
		out << n << '\n';
		out << s + 1 << ' ' << n_term << ' ';
		for (std::size_t i = 0; i < n; i++) {
			if (t[i]) {
				out << i + 1 << ' ';
			}
		}

		out << '\n';

		for (std::size_t i = 0; i < n; i++) {
			for (std::size_t j = 0; j < sigma; j++) {
				out << g[i][j] + 1 << ' ';
			}
			out << '\n';
		}

	}


	void build() {
		mark.resize(n, vi(n));
		std::queue<std::pair<int, int> > q;

		for (std::size_t i = 0; i < n; i++) {
			for (std::size_t j = 0; j < n; j++) {
				if (!mark[i][j] && t[i] != t[j]) {
					q.push({ i, j });
					mark[i][j] = mark[j][i] = 1;
				}
			}
		}
		while (!q.empty()) {
			int u = q.front().first;
			int v = q.front().second;
			q.pop();
			for (std::size_t tr = 0; tr < sigma; tr++) {
				for(int to1 : g_rev[u][tr])
					for(int to2 : g_rev[v][tr])
						if (!mark[to1][to2]) {
							q.push({ to1, to2 });
							mark[to1][to2] = mark[to2][to1] = 1;
						}
			}
		}
	}

	int n, m, s, n_term, sigma;
	vvi g, mark;
	std::vector<vvi> g_rev;
	vi t;

	void printgv(std::ostream &out) {
		out << "digraph finite_state_machine {\nrankdir=LR;\nsize=\"8,5\"\n";
		if(t[s])
			out << "node [shape = doublecircle]; S;\n", n_term--;
		else
			out << "node [shape = circle]; S;\n";
		if(n_term){
			out << "node [shape = doublecircle]; ";
			for(std::size_t i = 0; i < n; i++) {
				if(t[i] && i != s)
					out << "Q" << i + 1 << ' ';
			}
			out << ";\n";
		}

		out << "node [shape = point]; qi;\n qi -> S\n";
		out << "node [shape = circle];\n";
		for(std::size_t i = 0; i < n; i++) {
			for(std::size_t j = 0; j < sigma; j++) {
				if(i != s)
					out << "Q" << i + 1;
				else
					out << "S";

				out << " -> ";

				if(g[i][j] != s) 
					out << "Q" << g[i][j] + 1;
				else
					out << "S";
				out << "[ label = \"" << j + 1 << " \"];\n";
			}
		}
	
		out << "}";

	}
};

vi used;

void dfs(const automaton& a, int v) {
	used[v] = 1;
	for (std::size_t i = 0; i < a.sigma; i++) {
		if (!used[a.g[v][i]]) {
			dfs(a, a.g[v][i]);
		}
	}
}

automaton minimize(automaton& a) {
	a.build();
	used.resize(a.n);
	dfs(a, a.s);
	vi cmp(a.n, -1);
	vi member(1, a.s);

	for (std::size_t i = 0; i < a.n; i++) {
		if (!a.mark[a.s][i]) {
			cmp[i] = 0;
		}
	}

	int n_cmp = 1;

	for (std::size_t i = 0; i < a.n; i++) {
		if (!used[i])
			continue;
		if (cmp[i] == -1) {
			cmp[i] = n_cmp++;
			member.push_back(i);
			for (std::size_t j = i + 1; j < a.n; j++)
				if (!a.mark[i][j])
					cmp[j] = cmp[i];
		}
	}

	automaton a_new;
	a_new.n = n_cmp;
	a_new.s = 0;
	a_new.sigma = a.sigma;
	a_new.g.resize(n_cmp, vi(a.sigma, -1));
	a_new.t.resize(n_cmp);
	a_new.n_term = 0;
	a_new.m = 0;

	for (std::size_t i = 0; i < n_cmp; i++) {
		for (std::size_t j = 0; j < a.sigma; j++) {
			a_new.g[i][j] = cmp[a.g[member[i]][j]];
			if (a.g[member[i]][j] != -1)
				a_new.m++;
		}
	}

	for (std::size_t i = 0; i < n_cmp; i++) {
		if (a.t[member[i]] && !a_new.t[i]) {
			a_new.n_term++;
			a_new.t[i] = 1;
		}
	}

	a_new.s = 0;

	if (a.n <= n_cmp)
		return a;
	else
		return a_new;
}

int main(int argc, char* argv[]) {
	std::ifstream in(argv[1]);
	//std::ofstream out(argv[2]);
	std::ofstream out1(argv[2]);
	//std::ofstream out2(argv[4]);

	automaton a;
	a.read(in);
	automaton a_new = minimize(a);
	if(argc > 2)
		a_new.printgv(out1);
	else
		a_new.printgv(std::cout);
}