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
  if (first < 0 || last >= x.size() || last < first) {
    stop("invalid subsequence indices");
  }
  int n = 1 + last - first;
  sequence res(n);
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

RObject list_to_tibble(List x) {
  Environment pkg = Environment::namespace_env("tibble");
  Function f = pkg["as_tibble"];
  return(f(x));
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
  long int count;
  
  record_simple() {
    count = 1; 
  }
  
  void add_1() {
    if (count >= LONG_MAX) {
      stop("cannot increment this record count any higher");
    }
    count += 1;
  }
};

double compute_entropy(std::vector<double> x) {
  int n = x.size();
  double counter = 0;
  for (int i = 0; i < n; i ++) {
    double p = x[i];
    counter += p * log2(p);
  }
  return(- counter / n);
}

class record_decay: public record {
public:
  record_decay() {}
  std::vector<int> pos;
  std::vector<double> time;
  void insert(int pos_, double time_) {
    pos.push_back(pos_);
    time.push_back(time_);
  }
};

class symbol_prediction {
public:
  int symbol;
  int pos;
  double time;
  std::vector<double> distribution;
  double information_content;
  
  symbol_prediction(int symbol_, 
                    int pos_, 
                    double time_, 
                    std::vector<double> &distribution_) {
    int dist_size_ = distribution_.size();
    if (symbol_ > dist_size_) {
      std::cout << "symbol = " << symbol_ << ", distribution(n) = " << distribution_.size() << "\n";
      stop("observed symbol not compatible with distribution dimensions");
    }
    
    symbol = symbol_;
    pos = pos_;
    time = time_;
    distribution = distribution_;
    information_content = - log2(distribution[symbol]);
  }
};

class sequence_prediction {
public: 
  bool return_distribution;
  bool return_entropy;
  bool decay;
  
  std::vector<int> symbol;
  std::vector<int> pos;
  std::vector<double> time;
  std::vector<double> information_content;
  std::vector<double> entropy;
  std::vector<std::vector<double>> distribution;
  
  sequence_prediction(bool return_distribution_,
                      bool return_entropy_,
                      bool decay_) {
    return_distribution = return_distribution_;
    return_entropy = return_entropy_;
    decay = decay_;
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
    if (decay) {
      pos.push_back(x.pos);
      time.push_back(x.time);
    }
  }
  
  List as_list() {
    List x = List::create(Named("symbol") = symbol);
    if (decay) {
      x.push_back(pos, "pos");
      x.push_back(time, "time");
    }
    x.push_back(information_content, "information_content");
    if (return_entropy) {
      x.push_back(entropy, "entropy");
    }
    if (return_distribution) {
      x.push_back(distribution, "distribution");
    }
    return(x);
  }
  
  RObject as_tibble() {
    return(list_to_tibble(this->as_list()));
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
  
  int num_observations = 0;
  
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
  
  virtual ~ ppm() {};
  
  virtual void insert(sequence x, int pos, double time) {};
  
  sequence_prediction model_seq(sequence x,
                                NumericVector time = NumericVector(0),
                                bool train = true,
                                bool predict = true,
                                bool return_distribution = true,
                                bool return_entropy = true) {
    int n = x.size();
    bool decay = time.size() > 0;
    if (decay && x.size() != time.size()) {
      stop("time must either have length 0 or have length equal to x");
    }
    sequence_prediction result(return_entropy,
                               return_distribution,
                               decay);

    for (int i = 0; i < n; i ++) {
      int pos_i = num_observations;
      double time_i = decay? time[i] : 0;
      // Predict
      if (predict) {
        sequence context = (i < 1 || order_bound < 1) ? sequence() :
          subseq(x,
                 std::max(0, i - order_bound),
                 i - 1);
        result.insert(predict_symbol(x[i], context, pos_i, time_i));
      }
      // Train
      if (train) {
        for (int h = std::max(0, i - order_bound); h <= i; h ++) {
          this->insert(subseq(x, h, i), pos_i, time_i);
        }
        num_observations ++;
      }
    }
    return(result);
  }
  
  symbol_prediction predict_symbol(int symbol, sequence context,
                                   int pos, double time) {
    if (symbol < 0) {
      stop("symbols must be greater than or equal to 0");
    }
    if (symbol > alphabet_size - 1) {
      stop("symbols cannot exceed (alphabet_size - 1)");
    }
    
    std::vector<double> distribution(alphabet_size, 0.0);
    distribution[0] = 0.5;
    distribution[1] = 0.25;
    distribution[2] = 0.25;
    
    symbol_prediction out(symbol, pos, time, distribution);
    return(out);
  }
  
  std::vector<double> get_probability_distribution(int pos, double time) {
    
  }
  
  std::vector<double> get_smoothed_distribution(int order, int pos, double time,
                                                bool excluded) {
    
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
  
  ~ ppm_simple() {};
  
  void insert(sequence x, int pos, double time) {
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
  
  List as_list() {
    int n = data.size();
    List n_gram(n);
    NumericVector count(n); // NumericVector deals better with v long ints
    
    int i = 0;
    for(auto kv : data) {
      n_gram[i] = kv.first;
      count[i] = kv.second.count;
      i ++;
    } 
    
    List x = List::create(Named("n_gram") = n_gram,
                          Named("count") = count);
    return(x);
  }
  
  RObject as_tibble() {
    return(list_to_tibble(this->as_list()));
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
  
  ~ ppm_decay() {};
  
  void insert(sequence x, int pos, double time) {
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
  
  List as_list() {
    int n = data.size();
    List n_gram(n);
    List pos(n);
    List time(n);
    
    int i = 0;
    for(auto kv : data) {
      n_gram[i] = kv.first;
      pos[i] = kv.second.pos;
      time[i] = kv.second.time;
      i ++;
    } 
    
    List x = List::create(Named("n_gram") = n_gram,
                          Named("pos") = pos,
                          Named("time") = time);
    return(x);
  }
  
  RObject as_tibble() {
    return(list_to_tibble(this->as_list()));
  }
  
};

RCPP_EXPOSED_CLASS(record_decay)
  RCPP_EXPOSED_CLASS(ppm_decay)
  RCPP_EXPOSED_CLASS(symbol_prediction)
  RCPP_EXPOSED_CLASS(sequence_prediction)
  
  RCPP_MODULE(ppm) {
    class_<sequence_prediction>("sequence_prediction")
      .field("information_content", &sequence_prediction::information_content)
      .field("entropy", &sequence_prediction::entropy)
      .field("distribution", &sequence_prediction::distribution)
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
      .method("model_seq", &ppm::model_seq)
      .method("insert", &ppm::insert)
      ;

    class_<ppm_simple>("ppm_simple")
      .derives<ppm>("ppm")
      .constructor<int, int, bool, bool, bool, std::string>()
      .method("get_count", &ppm_simple::get_count)
      .method("as_tibble", &ppm_simple::as_tibble)
    ;

    class_<ppm_decay>("ppm_decay")
      .derives<ppm>("ppm")
      .constructor<int, int, List>()
      .method("get", &ppm_decay::get)
      .method("as_tibble", &ppm_decay::as_tibble)
      .method("as_list", &ppm_decay::as_list)
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
