#include <iostream>
#include <fstream>
#include <cassert>
#include <vector>
#include <map>
#include <string>
#include <set>
#include <algorithm>

using namespace std;

struct Feature
{
   string calling_function;
   string message;
   string nkind;
   string source_file;

   size_t source_location;
   string node_id;

   map<string, string> parameters;

   void read_header(const string& p) {
      if (!p.empty() && p[0] == '-' && p.size() > 20) {
         if (p.find("At:") != string::npos)
            calling_function = p.substr(14, p.size() - 24);
         else
            message = p.substr(10, p.size() - 20);
      }
   }

   void read_param(const string& p) {
      auto equal_sign_pos = p.find('=');
      if (equal_sign_pos != string::npos)
         parameters[p.substr(1, equal_sign_pos - 2)] = p.substr(equal_sign_pos + 2);
   }

   void read_nkind(const string& p) {
      auto space_sign_pos = p.find(' ');
      if (space_sign_pos != string::npos)
         nkind = p.substr(0, space_sign_pos);
      auto equal_sign_pos = p.find('=');
      if (equal_sign_pos != string::npos) {
         auto close_bracket_sign_pos = p.find(')', equal_sign_pos);
         if (close_bracket_sign_pos != string::npos)
            node_id = p.substr(equal_sign_pos + 1, (close_bracket_sign_pos - equal_sign_pos) - 1);
      }
   }

   Feature(vector<string>& params) {
      for (auto &p : params) {
         switch (p[0]) {
         case '-' :
            read_header(p);
            break;
         case ' ' :
            read_param(p);
            break;
         default:
            read_nkind(p);
            break;
         }
      }
   }

   // const Feature& operator=(const Feature& other) {

   // }

   string unique_id() const {
      return calling_function + message + nkind;
   }

   bool operator==(const Feature& other) const {
      return unique_id() == other.unique_id();
   }

   bool operator<(const Feature& other) const {
      return unique_id() < other.unique_id();
   }

   string get_param(const string& name) const {
      return parameters.at(name);
   }

   void printout(string& output) const {
      output.clear();
      output = "Calling function: " + calling_function + "\n";
      output += "Error message: " + message + "\n";
      output += "Nkind: " + nkind + "\n";
      output += "Node id: " + node_id + "\n";
      if (parameters.empty())
         return;
      auto it = parameters.begin();
      output += it->first + ": " + it->second;
      it++;
      for (; it != parameters.end(); it++) {
         output += "\n" + it->first + ": " + it->second;
      }
      output += "\n--------------------------------------------------------------------------------\n";
   }
};

template<typename Key, typename F>
void iterate_multiset(const multiset<Key>& m, const F& f) {
   if (m.empty())
      return;

   Key representative = *m.begin();
   f (representative);

   for (Key k : m)
      if (!(k == representative)) {
         representative = k;
         f (representative);
      }
}

int main(int argc, char** argv)
{
   assert(argc > 1);

   fstream input_file;
   input_file.open(argv[1]);
   assert(input_file.is_open());

   multiset<Feature> unsupported_features;
   using Instance = pair<Feature, size_t>;
   vector<Instance> instances;
   vector<string> params;

   string line;
   getline(input_file, line);
   unsigned long count=0;
   cerr << "Scanning " << argv[1] << "...";
   while (true) {
      ++count;
      if (count % 1000 == 0) cerr << ".";
      if (line.find("----------At:") == 0) {
         params.push_back(line);
         assert(getline(input_file, line));
         params.push_back(line);
         assert(getline(input_file, line));
         params.push_back(line);
         while (getline(input_file, line) && !line.empty() && line[0] == ' ')
         {
            ++count;
            if (count % 1000 == 0) cerr << ".";
            params.push_back(line);
         }
         unsupported_features.emplace(params);
         continue;
      }
      if (!getline(input_file, line))
         break;
   }
   cerr << endl;

   iterate_multiset(unsupported_features, [&] (auto& k) {
         instances.push_back({k, unsupported_features.count(k)});
      });

   sort(instances.begin(), instances.end(), [&] (const auto& l, const auto& r) {
         return l.second == r.second ? l.first < r.first : l.second > r.second;
      });

   string temp;
   cout << "Unsupported features: " << unsupported_features.size() << endl;
   for (const auto& p : instances) {
      cout << "--------------------------------------------------------------------------------\n";
      cout << "Occurs: " << p.second << " times\n";
      p.first.printout(temp);
      cout << temp;
   }

   return 0;
}
