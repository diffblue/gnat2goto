#include <iostream>
#include <fstream>
#include <cassert>
#include <vector>
#include <map>
#include <string>
#include <set>

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

   bool operator=(const Feature& other) const {
      return parameters == other.parameters;
   }

   bool operator<(const Feature& other) const {
      return parameters < other.parameters;
   }

   string get_param(const string& name) const {
      return parameters.at(name);
   }
};

int main(int argc, char** argv)
{
   assert(argc > 1);

   fstream input_file;
   input_file.open(argv[1]);
   assert(input_file.is_open());

   set<Feature> unsupported_features;
   vector<string> params;

   string line;
   getline(input_file, line);
   while (true) {
      if (line.find("----------At:") == 0) {
         params.push_back(line);
         assert(getline(input_file, line));
         params.push_back(line);
         assert(getline(input_file, line));
         params.push_back(line);
         while (getline(input_file, line) && !line.empty() && line[0] == ' ')
            params.push_back(line);
         unsupported_features.emplace(params);
         continue;
      }
      if (!getline(input_file, line))
         break;
   }

   cout << "Unsupported features: " << unsupported_features.size() << endl;
   for (auto it = unsupported_features.begin(); it != unsupported_features.end(); it++) {
      cout << "sloc: " << it->get_param("Sloc") << endl;
      cout << "node_id: " << it->node_id << endl;
   }

   return 0;
}
