// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
using namespace Rcpp;

#include <iostream>
#include <string>
#include <unordered_map>
#include <boost/functional/hash.hpp>

typedef std::vector<long int> n_gram;

template <typename Container> // we can make this generic for any container [1]
struct container_hash {
  std::size_t operator() (Container const& c) const {
    return boost::hash_range(c.begin(), c.end());
  }
};

class record {
  
};

class record_simple: public record {
public: 
  record_simple() : count(1) {}
  void add_1() {
    if (count >= LONG_MAX) {
      stop("cannot increment this record count any higher");
    }
    count += 1;
  }
  long int count;
};

class record_decay: public record {
public:
  record_decay() {}
  std::vector<long int> pos;
  std::vector<double> time;
  void insert(long int pos_, double time_) {
    pos.push_back(pos_);
    time.push_back(time_);
  }
};

class ppm {
public:
  int alphabet_size;
  int order_bound;
  bool shortest_deterministic;
  bool exclusion;
  bool update_exclusion;
  std::string escape;
  
  ppm(int alphabet_size_,
      int order_bound_,
      bool shortest_deterministic_,
      bool exclusion_,
      bool update_exclusion_,
      std::string escape_
  ) {
    alphabet_size = alphabet_size_;
    order_bound = order_bound_;
    shortest_deterministic = shortest_deterministic_;
    exclusion = exclusion_;
    update_exclusion = update_exclusion_;
    escape = escape_;
  }
};

class ppm_simple: public ppm {
public:
  std::unordered_map<n_gram, 
                     record_simple,
                     container_hash<n_gram>> data;
  
  ppm_simple(
    int alphabet_size_,
    int order_bound_,
    bool shortest_deterministic_,
    bool exclusion_,
    bool update_exclusion_,
    std::string escape_
  ) : ppm(
      alphabet_size_,
      order_bound_, 
      shortest_deterministic_, 
      exclusion_, 
      update_exclusion_, 
      escape_){
    data = {};
  }
  
  void insert(n_gram x) {
    std::unordered_map<n_gram, record_simple, container_hash<n_gram>>::const_iterator target = data.find(x);
    if (target == data.end()) {
      data[x] = record_simple();
    } else {
      data[x].add_1();
    }
  }
  
  long int get_count(n_gram x) {
    std::unordered_map<n_gram, record_simple, container_hash<n_gram>>::const_iterator target = data.find(x);
    if (target == data.end()) {
      return(0);
    } else {
      return(target->second.count);
    }
  }
};

class ppm_decay: public ppm {
public:
  std::unordered_map<n_gram, record_decay, container_hash<n_gram>> data;
  
  double buffer_length_time;
  int buffer_length_items;
  double buffer_weight;
  double stm_half_life;
  double stm_weight;
  double ltm_weight;
  double noise;
  
  ppm_decay(
    int alphabet_size_,
    int order_bound_,
    List decay_par
  ) : ppm (
      alphabet_size_,
      order_bound_,
      false, // shortest_deterministic
      false, // exclusion
      false, // update_exclusion
      "A" // escape
  ) {
    data = {};
    buffer_length_items = decay_par["buffer_length_items"];
    buffer_weight = decay_par["buffer_weight"];
    stm_half_life = decay_par["stm_half_life"];
    stm_weight = decay_par["stm_weight"];
    ltm_weight = decay_par["ltm_weight"];
    noise = decay_par["noise"];
  }
  
  void insert(n_gram x, long int pos, double time) {
    std::unordered_map<n_gram, 
                       record_decay, 
                       container_hash<n_gram>>::const_iterator target = data.find(x);
    if (target == data.end()) {
      record_decay record;
      record.insert(pos, time);
      data[x] = record;
    } else {
      data[x].insert(pos, time);
    }
  }
  
  record_decay get(n_gram x) {
    std::unordered_map<n_gram, 
                       record_decay, 
                       container_hash<n_gram>>::const_iterator target = data.find(x);
    if (target == data.end()) {
      record_decay blank;
      return(blank);
    } else {
      return(target->second);
    }
  }
};

RCPP_EXPOSED_CLASS(record_decay)
  RCPP_EXPOSED_CLASS(ppm_decay)
  
  RCPP_MODULE(ppm) {
    class_<ppm>("ppm")
    // ppm class cannot be instantiated directly in R
    .field("alphabet_size", &ppm::alphabet_size)
    .field("order_bound", &ppm::order_bound)
    .field("shortest_deterministic", &ppm::shortest_deterministic)
    .field("exclusion", &ppm::exclusion)
    .field("update_exclusion", &ppm::update_exclusion)
    .field("escape", &ppm::escape)
    ;
    
    class_<ppm_simple>("ppm_simple")
      .derives<ppm>("ppm")
      .constructor<int, int, bool, bool, bool, std::string>()
      .method("insert", &ppm_simple::insert)
      .method("get_count", &ppm_simple::get_count)
    ;
    
    class_<ppm_decay>("ppm_decay")
      .derives<ppm>("ppm")
      .constructor<int, int, List>()
      .method("insert", &ppm_decay::insert)
      .method("get", &ppm_decay::get)
      .field("buffer_length_time", &ppm_decay::buffer_length_time)
      .field("buffer_length_items", &ppm_decay::buffer_length_items)
      .field("buffer_weight", &ppm_decay::buffer_weight)
      .field("stm_half_life", &ppm_decay::stm_half_life)
      .field("stm_weight", &ppm_decay::stm_weight)
      .field("ltm_weight", &ppm_decay::ltm_weight)
      .field("noise", &ppm_decay::noise)
    ;
    
    class_<record_decay>("record_decay")
      .constructor()
      .field("pos", &record_decay::pos)
      .field("time", &record_decay::time)
      .method("insert", &record_decay::insert)
    ;
  }
