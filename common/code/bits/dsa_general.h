/*
 Windows:
 - find which g++, and use the following command
 - g++ -v -E -x c++ - < nul
   Now look for lines `#include <...> search starts here:`
   you'll see paths like:
   C:/msys64/mingw64/include/c++/14.2.0
   C:/msys64/mingw64/include/c++/14.2.0/x86_64-w64-mingw32
- now based on the above include path, the bits folder for g++ is,
   C:\msys64\mingw64\include\c++\14.2.0\bits
- with that as the target folder, link this file with the command
```
New-Item -ItemType SymbolicLink -Path "C:\msys64\mingw64\include\c++\14.2.0\bits\dsa_general.h" -Target C:\Users\sreddy\dotfiles\common\code\bits\dsa_general.h
```
 */

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
