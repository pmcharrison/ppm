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

sequence last_n (sequence x, int n) {
  if (n < 0) {
    stop("n cannot be less than 0");
  }
  int original_length = static_cast<int>(x.size());
  if (n > original_length) {
    stop("cannot excise more elements than the sequence contains");
  }
  if (n == 0) {
    sequence res(0);
  } else {
    sequence res(n);
    for (int i = 0; i < n; i ++) {
      res[i] = x[i + original_length - n];
    }
  }
  return res;
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

std::vector<double> normalise_distribution(std::vector<double> x) {
  double total = 0;
  int n = static_cast<int>(x.size());
  for (int i = 0; i < n; i ++) {
    total += x[i];
  }
  for (int i = 0; i < n; i ++) {
    x[i] = x[i] / total;
  }
  return x;
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
  int model_order;
  std::vector<double> distribution;
  double information_content;
  
  symbol_prediction(int symbol_, 
                    int pos_, 
                    double time_, 
                    int model_order_,
                    std::vector<double> &distribution_) {
    int dist_size_ = distribution_.size();
    if (symbol_ > dist_size_) {
      std::cout << "symbol = " << symbol_ << ", distribution(n) = " << distribution_.size() << "\n";
      stop("observed symbol not compatible with distribution dimensions");
    }
    
    symbol = symbol_;
    pos = pos_;
    time = time_;
    model_order = model_order_;
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
  std::vector<int> model_order;
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
    model_order.push_back(x.model_order);
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
    x.push_back(model_order, "model_order");
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

class model_order {
public:
  int chosen;
  bool deterministic_any;
  int deterministic_shortest;
  bool deterministic_is_selected;
  
  model_order(int chosen_,
              bool deterministic_any_,
              int deterministic_shortest_, 
              bool deterministic_is_selected_) {
    chosen = chosen_;
    deterministic_any = deterministic_any_;
    deterministic_shortest = deterministic_shortest_;
    deterministic_is_selected = deterministic_is_selected_;
  }
};

class escape_method {
public:
  double k;
  
  escape_method(double k_) {
    k = k_;
  }
  
  virtual double lambda(std::vector<double> counts, 
                        double context_count) {
    return 0;
  }
  virtual double modify_count(double count) {
    if (this.k == 0 || count == 0) {
      return count;
    } else {
      return std::max(count + k, 0);
    }
  }
  
private: 
  int count_positive_values(std::vector<double> x) {
    int n = static_cast<int>(x.size());
    int res = 0;
    for (int i = 0; i < n; i ++) {
      if (x[i] > 0) {
        res ++;
      }
    }
    return res
  }
};

class escape_a: public escape_method {
public:
  escape_a() : escape_method(0) {}
  
  double lambda(std::vector<double> counts, 
                double context_count) {
    return context_count / (context_count + 1);
  }
};

class escape_b: public escape_method {
public:
  escape_b() : escape_method(-1) {}
  
  double lambda(std::vector<double> counts, 
                double context_count) {
    int num_distinct_symbols = this->count_positive_values(counts);
    return context_count / (context_count + num_distinct_symbols);
  }
};

class escape_c: public escape_method {
public:
  escape_c() : escape_method(0) {}
  
  double lambda(std::vector<double> counts, 
                double context_count) {
    int num_distinct_symbols = this->count_positive_values(counts);
    return context_count / (context_count + num_distinct_symbols);
  }
};

class escape_d: public escape_method {
public:
  escape_d() : escape_method(- 0.5) {}
  
  double lambda(std::vector<double> counts, 
                double context_count) {
    int num_distinct_symbols = this->count_positive_values(counts);
    return context_count / (context_count + num_distinct_symbols / 2);
  }
};

class escape_ax: public escape_method {
public:
  escape_ax() : escape_method(0) {}
  
  double lambda(std::vector<double> counts, 
                double context_count) {
    // We generalise the definition of singletons to decayed counts between
    // 0 and 1. This is a bit hacky though, and the escape method
    // should ultimately be reconfigured for new decay functions.
    return context_count / (context_count + num_singletons(counts));
  }
private:
  int num_singletons(std::vector<double> x) {
    int n = static_cast<int>(x.size());
    int res = 0;
    for (int i = 0; i < n; i ++) {
      if (x[i] > 0 && x[i] <= 1) {
        res ++
      }
    }
    return res;
  }
};

class ppm {
public:
  int alphabet_size;
  int order_bound;
  bool shortest_deterministic;
  bool exclusion;
  bool update_exclusion;
  escape_method escape;
  
  int num_observations = 0;
  
  ppm(int alphabet_size_,
      int order_bound_,
      bool shortest_deterministic_,
      bool exclusion_,
      bool update_exclusion_,
      escape_method escape_
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
  
  virtual double get_weight(sequence n_gram, int pos, double time) {
    return 0;
  };
  
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
    
    model_order model_order = this->get_model_order(context, pos, time);
    std::vector<double> dist = get_probability_distribution(context,
                                                            model_order,
                                                            pos,
                                                            time);
    
    symbol_prediction out(symbol, pos, time, model_order.chosen, dist);
    return(out);
  } 
  
  std::vector<double> get_probability_distribution(sequence context,
                                                   model_order model_order,
                                                   int pos, 
                                                   double time) {
    std::vector<bool> excluded(alphabet_size, false);
    std::vector<double> dist = get_smoothed_distribution(
      context,
      model_order,
      model_order.chosen,
      pos,
      time,
      excluded
    );
    return normalise_distribution(dist);
  }
  
  std::vector<double> get_smoothed_distribution(context,
                                                model_order model_order, 
                                                int order,
                                                int pos, 
                                                double time,
                                                std::vector<bool> excluded) {
    if (order == -1) {
      return get_order_minus_1_distribution(excluded);
    } else {
      bool update_excluded = this.update_exclusion;
      if (order == model_order.chosen &&
          this.shortest_deterministic &&
          this.update_exclusion &&
          model_order.det_is_selected) {
        update_excluded = false;
      }
      
      std::vector<int> n_gram = last_n(context, order);
      n_gram.resize(order + 1);
      
      std::vector<double> counts(this.alphabet_size);
      for (int i = 0; i < this.alphabet_size; i ++) {
        n_gram[order] = i;
        counts[i] = this->get_weight(n_gram, pos, time, update_excluded);
        counts[i] = this->escape.modify_count(counts[i]);
      }
      
      double context_count = 0;
      for (int i = 0; i < this.alphabet_size; i ++) {
        if (!excluded[i]) {
          context_count += counts[i];
        }
      }

      double lambda = get_lambda(counts, context_count);
      std::vector<double> alphas = get_alphas(lambda, counts, context_count);
      
      if (this.exclusion) {
        for (int i = 0; i < alphabet_size; i ++) {
          if (alphas[i] > 0) {
            excluded[i] = true;
          }
        }
      }
      
      std::vector<double> lower_order_distribution = get_smoothed_distribution(
        context, model_order, order - 1, pos, time, excluded);
      
      std::vector<double> res(this.alphabet_size);
      for (int i = 0; i < this.alphabet_size; i ++) {
        res[i] = alphas[i] + (1 - lambda) * lower_order_distribution[i];
      }
    }
  }
  
  std::vector<double>get_alphas(double lambda, 
                                std::vector<double> counts, 
                                double context_count) {
    if (lambda > 0) {
      std::vector<double> res(this.alphabet_size);
      for (int i = 0; i < this.alphabet_size; i ++) {
        res[i] = lambda * counts[i] / context_count;
      }
    } else {
      std::vector<double> res(this.alphabet_size, 0);
    }
    return res;
  }
  
  // The need to capture situations where the context_count is 0 is
  // introduced by Pearce (2005)'s decision to introduce exclusion
  // (see 6.2.3.3), though the thesis does not mention
  // this explicitly.
  double get_lambda(std::vector<double> counts, 
                    double context_count) {
    if (context_count <= 0) {
      return 0;
    } else {
      return this->escape.lambda(counts, context_count);
    }
  }
  
  std::vector<double> get_order_minus_1_distribution(std::vector<bool> excluded) {
    int num_observed_symbols = 0;
    for (int i = 0; i < this.alphabet_size; i ++) {
      if (excluded[i]) {
        num_observed_symbols ++;
      } 
    }
    double p = 1 / (this.alphabet_size + 1 - num_observed_symbols);
    std::vector<double> res(this.alphabet_size, p);
    return res
  }
  
  model_order get_model_order(sequence context, int pos, double time) {
    // int longest_available = this->get_longest_context(context);
    int chosen = static_cast<int>(context.size());
    
    int det_shortest = - 1;
    int det_any = false;
    int det_is_selected = false;
    
    if (shortest_deterministic) {
      int det_shortest = this->get_shortest_deterministic_context(context,
                                                                  pos,
                                                                  time);
      bool det_any = det_shortest >= 0;
      if (det_any) {
        if (det_shortest < chosen) {
          det_is_selected = true;
          chosen = det_shortest;
        }
      }
    }
    
    return(model_order(chosen, det_any, det_shortest, det_is_selected));
  }
  
  int get_shortest_deterministic_context(sequence context, int pos, double time) {
    int len = static_cast<int>(context.size());
    int res = -1;
    for (int order = 0; order <= std::min(len, order_bound); order ++) {
      sequence effective_context = order == 0 ? sequence() : subseq(context, 
                                                         len - order, len - 1);
      if (is_deterministic_context(effective_context, pos, time)) {
        res = order;
        break;
      }
    }
    return(res);
  }
  
  bool is_deterministic_context(sequence context, int pos, double time) {
    int num_continuations = 0;
    for (int i = 0; i < alphabet_size; i ++) {
      sequence n_gram = context;
      n_gram.push_back(i);
      double weight = this->get_weight(n_gram, pos, time);
      if (weight > 0) {
        num_continuations ++;
        if (num_continuations > 1) {
          break;
        }
      }
    }
    return num_continuations == 1;
  }
  
  // I don't think get_longest_context is necessary.
  // We can just start with the full context we have available,
  // and the model will escape down to lower levels if 
  // it doesn't find any counts.
  
  // int get_longest_context(sequence context) {
  //   int context_len = static_cast<int>(context.size());
  //   int max_order = std::min(order_bound, context_len);
  // 
  //   
  //   
  //   for (int order = max_order; order >= 0; order --) {
  //     sequence x = order == 0 ? sequence() : subseq(context, 
  //                                        context_len - order, 
  //                                        context_len - 1);
  //     bool 
  //     
  //     
  //     // Check that context exists in the tree
  //     
  //     // Check that context has a continuation
  //     return(order);
  //   }
  //   return(- 1);
  //   
  // }
  
  
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
    escape_method escape_
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
  
  double get_weight(sequence n_gram, int pos, double time) {
    return static_cast<double>(this->get_count(n_gram));
  };
  
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
      escape_a() // escape
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
  
  double get_weight(sequence n_gram, int pos, double time) {
    record_decay record = this->get(n_gram);
    double weight = static_cast<double>(record.pos.size());
    return(weight);
  };
  
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
