#include <Rcpp.h>
using namespace Rcpp;

#include <iostream>
#include <string>
#include <unordered_map>

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

//' @export
// [[Rcpp::export]]
std::unordered_map<std::string, std::string> rcpp_hello_world() {

  // Create an unordered_map of three strings (that map to strings)
  std::unordered_map<std::string, std::string> u = {
    {"RED","#FF0000"},
    {"GREEN","#00FF00"},
    {"BLUE","#0000FF"}
  };
  
  // Iterate and print keys and values of unordered_map
  for( const auto& n : u ) {
    std::cout << "Key:[" << n.first << "] Value:[" << n.second << "]\n";
  }
  
  // Add two new entries to the unordered_map
  u["BLACK"] = "#000000";
  u["WHITE"] = "#FFFFFF";
  
  // Output values by key
  std::cout << "The HEX of color RED is:[" << u["RED"] << "]\n";
  std::cout << "The HEX of color BLACK is:[" << u["BLACK"] << "]\n";
  
  return(u);
}

class ppm_record {
  
};

class ppm_record_count: public ppm_record {
public: 
  ppm_record_count() : count(1) {}
  void add_1() {
    if (count >= ULONG_MAX) {
      stop("cannot increment this record count any higher");
    }
    count += 1;
  }
  unsigned long int count;
};

class ppm_model {
public:
  ppm_model(bool when_, int max_order_bound_) : when(when_), max_order_bound(max_order_bound_) {
    data = {};
  }
  
  bool when;
  int max_order_bound;
  std::unordered_map<unsigned long int, ppm_record_count> data;
  
  void insert(unsigned long int token) {
    std::unordered_map<unsigned long int, ppm_record_count>::const_iterator target = data.find(token);
    if (target == data.end()) {
      data[token] = ppm_record_count();
      std::cout << data[token].count << "\n";
    } else {
      data[token].add_1();
      std::cout << data[token].count << "\n";
    }
  }
  
  unsigned long int get_count(unsigned long int token) {
    std::unordered_map<unsigned long int, ppm_record_count>::const_iterator target = data.find(token);
    if (target == data.end()) {
      return(0);
    } else {
      return(target->second.count);
    }
  }
};

RCPP_MODULE(ppm_model) {
  class_<ppm_model>("ppm_model")
  .constructor<bool, int>()
  .field("when", &ppm_model::when )
  .field("max_order_bound", &ppm_model::max_order_bound)
  .method("insert", &ppm_model::insert)
  .method("get_count", &ppm_model::get_count)
  //.field("data", &ppm_model::data)
  ;
}

class Uniform {
public:
  Uniform(double min_, double max_) : min(min_), max(max_) {}
  NumericVector draw(int n) const {
    RNGScope scope;
    return runif( n, min, max );
  }
  double min, max;
};
double uniformRange( Uniform* w) {
  return w->max - w->min;
}
RCPP_MODULE(unif_module) {
  class_<Uniform>( "Uniform" )
  .constructor<double,double>()
  .field( "min", &Uniform::min )
  .field( "max", &Uniform::max )
  .method( "draw", &Uniform::draw )
  .method( "range", &uniformRange )
  ;
}
