// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
using namespace Rcpp;

#include <cmath>
#include <iostream>
#include <algorithm>
#include <iterator>
#include <string>
#include <unordered_map>
#include <boost/functional/hash.hpp>

typedef std::vector<int> sequence;

sequence subseq(sequence x, int first, int last) {
  int n = x.size();
  if (first < 0 || last >= n || last < first) {
    stop("invalid subsequence indices");
  }
  sequence res(1 + last - first);
  for (int j = 0; j < n; j ++) {
    res[j] = x[j + first];
  }
  return(res);
}

void print(sequence x) {
  for (int j = 0; j < x.size(); j ++) {
    if (j > 0) {
      std::cout << " ";
    }
    std::cout << x[j];
  }
  std::cout << "\n";
}

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

class symbol_prediction {
public:
  int symbol;
  double information_content;
  std::vector<double> distribution;
  
  symbol_prediction() {
    symbol = 3;
    information_content = 5.76;
    distribution.push_back(0.5);
    distribution.push_back(0.25);
    distribution.push_back(0.25);
  }
};

double compute_entropy(std::vector<double> x) {
  int n = x.size();
  double counter = 0;
  for (int i = 0; i < n; i ++) {
    double p = x[i];
    counter += p * log2(p);
  }
  return(counter / n);
}

class sequence_prediction {
public: 
  bool return_distribution;
  bool return_entropy;
  
  std::vector<int> symbol;
  std::vector<double> information_content;
  std::vector<double> entropy;
  std::vector<std::vector<double>> distribution;
  
  sequence_prediction(bool return_distribution_,
                      bool return_entropy_) {
    return_distribution = return_distribution_;
    return_entropy = return_entropy_;
  }
  
  void insert(symbol_prediction x) {
    symbol.push_back(x.symbol);
    information_content.push_back(x.information_content);
    if (return_entropy) {
      entropy.push_back(compute_entropy(x.distribution));
    }
    if (return_distribution) {
      distribution.push_back(x.distribution);
    }
  }
  
  List as_list() {
    List x = List::create(Named("symbol") = symbol,
                          Named("information_content") = information_content);
    if (return_entropy) {
      x.push_back(entropy, "entropy");
    }
    if (return_distribution) {
      x.push_back(distribution, "distribution");
    }
    return(x);
  }
  
  RObject as_tibble() {
    Environment pkg = Environment::namespace_env("tibble");
    Function f = pkg["as_tibble"];
    List x = this->as_list();
    return(f(x));
  }
};

class ppm_simple: public ppm {
public:
  std::unordered_map<sequence, 
                     record_simple,
                     container_hash<sequence>> data;
  
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
  
  void insert(sequence x) {
    std::unordered_map<sequence, record_simple, container_hash<sequence>>::const_iterator target = data.find(x);
    if (target == data.end()) {
      data[x] = record_simple();
    } else {
      data[x].add_1();
    }
  }
  
  long int get_count(sequence x) {
    std::unordered_map<sequence, record_simple, container_hash<sequence>>::const_iterator target = data.find(x);
    if (target == data.end()) {
      return(0);
    } else {
      return(target->second.count);
    }
  }
  
  sequence_prediction model_seq(sequence x,
                                bool train = true,
                                bool predict = true,
                                bool return_distribution = true,
                                bool return_entropy = true) {
    int n = x.size();
    sequence_prediction result(return_entropy, 
                               return_distribution);
    for (int i = 0; i <= n; i ++) { // predicting symbol i (inc. terminal)
      if (train && i > 0) {
        int min = std::max(0, i - order_bound);
        int max = i - 1;
        for (int j = min; j <= max; j ++) {
          sequence n_gram(x.begin() + j,
                          x.begin() + max + 1);
          this->insert(n_gram);
        }
      }
      if (predict && i < n) {
        int symbol = x[i];
        
        sequence context = i < 1 ? sequence() :
          subseq(x, 
                 std::max(0, i - order_bound), 
                 i - 1);
        
        symbol_prediction y = predict_symbol(symbol, context);
        result.insert(predict_symbol(symbol, context));
        // return(this->predict_symbol(symbol, context));
        // return(1);
        
        // std::vector<float> y;
        // y.push_back(1);
        // // return(Rcpp::wrap(y));
      }
    }
    // symbol_prediction out;
    return(result);
    // return(result);
  }
  
  symbol_prediction predict_symbol(int symbol, sequence context) {
    // return(symbol_prediction({1, 2, 3, 4}));
    symbol_prediction out;
    return(out);
  }
};

class ppm_decay: public ppm {
public:
  std::unordered_map<sequence, record_decay, container_hash<sequence>> data;
  
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
  
  void insert(sequence x, long int pos, double time) {
    std::unordered_map<sequence, 
                       record_decay, 
                       container_hash<sequence>>::const_iterator target = data.find(x);
    if (target == data.end()) {
      record_decay record;
      record.insert(pos, time);
      data[x] = record;
    } else {
      data[x].insert(pos, time);
    }
  }
  
  record_decay get(sequence x) {
    std::unordered_map<sequence, 
                       record_decay, 
                       container_hash<sequence>>::const_iterator target = data.find(x);
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
  RCPP_EXPOSED_CLASS(symbol_prediction)
  RCPP_EXPOSED_CLASS(sequence_prediction)
  
  RCPP_MODULE(ppm) {
    class_<symbol_prediction>("symbol_prediction")
    .constructor()
    .field("distribution", &symbol_prediction::distribution)
    ;
    
    class_<sequence_prediction>("sequence_prediction")
      .field("information_content", &sequence_prediction::information_content)
      .field("entropy", &sequence_prediction::entropy)
      .field("distribution", &sequence_prediction::distribution)
      .method("as_list", &sequence_prediction::as_list)
      .method("as_tibble", &sequence_prediction::as_tibble)
    ;
    
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
      .method("model_seq", &ppm_simple::model_seq)
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
