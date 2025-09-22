#include <bits/stdc++.h>
using namespace std;

// --- TYPE DEFINITIONS ----
typedef long long ll;

typedef pair<int, int> pii;
typedef pair<ll, ll> pll;

typedef vector<int> vi;
typedef vector<bool> vb;
typedef vector<string> vs;
typedef vector<vector<int>> vvi;
typedef vector<pii> vpii;

// --- MACROS ----
#define PB push_back
#define loop(i, a, b) for (int i = a; i < b; i++)
#define ff first
#define ss second

void pvi(const vi &a) {
  for (int num : a)
    cout << num << " ";
  cout << endl;
}

void pvvi(const vvi &a) {
  for (auto vec : a) {
    pvi(vec);
  }
}

void pvb(const vb &a) {
  for (auto b : a)
    cout << b << " ";
  cout << endl;
}

void pvp(const vpii &a) {
  for (int i = 0; i < a.size(); i++) {
    auto p = a[i];
    if (i != 0)
      cout << ", ";
    cout << "{" << p.first << "," << p.second << "}";
    cout << endl;
  }
}

void pumii(const unordered_map<int, int> &hm) {
  for (auto [k, v] : hm)
    cout << k << ":" << v << endl;
}
void pmii(const map<int, int> &hm) {
  for (auto [k, v] : hm)
    cout << k << ":" << v << endl;
}
