// #include "stdc++.h"
#include <bits/stdc++.h>
#include <iostream>
#include <map>
#include <set>
#include <unordered_set>
using namespace std;

// --- TYPE DEFINITIONS ----
typedef long long ll;

typedef pair<int, int> pii;
typedef pair<ll, ll> pll;

typedef vector<int> vi;
typedef vector<char> vc;
typedef vector<bool> vb;
typedef vector<string> vs;
typedef vector<vector<int>> vvi;
typedef vector<vector<string>> vvs;
typedef vector<vector<char>> vvc;
typedef vector<vector<bool>> vvb;
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

void pvc(const vc &a) {
  for (auto c : a)
    cout << c << " ";
  cout << endl;
}

void pvs(const vs &a) {
  for (auto s : a)
    cout << s << " ";
  cout << endl;
}

void pvvi(const vvi &a) {
  for (auto vec : a)
    pvi(vec);
}

void pvvc(const vvc &a) {
  for (auto vc : a)
    pvc(vc);
}

void pvvs(const vvs &a) {
  for (auto vs : a)
    pvs(vs);
}

void pvb(const vb &a) {
  for (auto b : a)
    cout << b << " ";
  cout << endl;
}

void pvvb(const vvb &a) {
  for (auto vec : a)
    pvb(vec);
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
