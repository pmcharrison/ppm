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

class record {
  
};

class record_simple: public record {
public: 
  record_simple() : count(1) {}
  void add_1() {
    if (count >= ULONG_MAX) {
      stop("cannot increment this record count any higher");
    }
    count += 1;
  }
  unsigned long int count;
};

class record_decay: public record {
public:
  record_decay() {}
  std::vector<unsigned long int> pos;
  std::vector<double> time;
  void insert(unsigned long int pos_, double time_) {
    pos.push_back(pos_);
    time.push_back(time_);
  }
};

class ppm {
  
};

class ppm_simple: public ppm {
public:
  ppm_simple(int max_order_bound_) : max_order_bound(max_order_bound_) {
    data = {};
  }
  
  int max_order_bound;
  std::unordered_map<unsigned long int, record_simple> data;
  
  void insert(unsigned long int token) {
    std::unordered_map<unsigned long int, record_simple>::const_iterator target = data.find(token);
    if (target == data.end()) {
      data[token] = record_simple();
    } else {
      data[token].add_1();
    }
  }
  
  unsigned long int get_count(unsigned long int token) {
    std::unordered_map<unsigned long int, record_simple>::const_iterator target = data.find(token);
    if (target == data.end()) {
      return(0);
    } else {
      return(target->second.count);
    }
  }
};

class ppm_decay: public ppm {
public:
  ppm_decay(int max_order_bound_) : max_order_bound(max_order_bound_) {
    data = {};
  }
  
  int max_order_bound;
  std::unordered_map<unsigned long int, record_decay> data;
  
  void insert(unsigned long int token, unsigned long int pos, double time) {
    std::unordered_map<unsigned long int, record_decay>::const_iterator target = data.find(token);
    if (target == data.end()) {
      record_decay record;
      record.insert(pos, time);
      data[token] = record;
    } else {
      data[token].insert(pos, time);
    }
  }
  
  record_decay get_record(unsigned long int token) {
    std::unordered_map<unsigned long int, record_decay>::const_iterator target = data.find(token);
      record_decay blank;
      return(blank);
    // if (target == data.end()) {
    //   record_decay blank;
    //   return(blank);
    // } else {
    //   return(target->second);
    // }
  }
};

RCPP_MODULE(ppm_simple) {
  class_<ppm_simple>("ppm_simple")
  .constructor<int>()
  .field("max_order_bound", &ppm_simple::max_order_bound)
  .method("insert", &ppm_simple::insert)
  .method("get_count", &ppm_simple::get_count)
  ;
}

RCPP_MODULE(ppm_decay) {
  class_<ppm_decay>("ppm_decay")
  .constructor<int>()
  .field("max_order_bound", &ppm_decay::max_order_bound)
  .method("insert", &ppm_decay::insert)
  // .method("get_record", &ppm_decay::get_record)
  ;
}

RCPP_MODULE(record_decay) {
  class_<record_decay>("record_decay")
  .constructor()
  .field("pos", &record_decay::pos)
  .field("time", &record_decay::time)
  .method("insert", &record_decay::insert)
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
