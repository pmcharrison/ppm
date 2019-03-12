context("test-ppmdecay")

library(magrittr)

# These regression tests compare model outputs with those 
# from the 'PPMdecay' package.

test_that("abra", {
  abra <- c("a", "b", "r", "a", "c", "a", "d", "a", "b", "r", "a") %>% 
    factor() %>% 
    as.integer() %>% 
    magrittr::subtract(1L)
  
  m <- new_ppm_decay(alphabet_size = 5L, 
                     order_bound = 10L,
                     buffer_length_time = 3, 
                     buffer_length_items = 2, 
                     buffer_weight = 0.8,
                     stm_half_life = 2,
                     stm_weight = 0.3, 
                     ltm_weight = 0.05, 
                     noise = 0)
  
  lapply(0:1, function(i) {
    model_seq(m, abra, time = 0:10 + i * 11)
  }) %>% do.call(rbind, .) %>% `$`(distribution) %>% 
    expect_equal(
      list(c(0.2, 0.2, 0.2, 0.2, 0.2), 
           c(0.591836734693878, 0.102040816326531, 0.102040816326531, 0.102040816326531, 0.102040816326531), 
           c(0.397260273972603, 0.397260273972603, 0.0684931506849315, 0.0684931506849315, 0.0684931506849315), 
           c(0.147904920371259, 0.363393489841669, 0.0626540499727015, 0.0626540499727015, 0.363393489841669), 
           c(0.324720602626979, 0.305524550586097, 0.0474044675367852, 0.0474044675367852, 0.274945911713354),
           c(0.371634645282067, 0.114903935846202, 0.325094062394133, 0.0560507004127815, 0.132316656064816), 
           c(0.212754603821084, 0.123029293122506, 0.581402207682115, 0.0271520968440312, 0.0556617985302639), 
           c(0.388611953842548, 0.0877829446703387, 0.123716836564072, 0.303964823336098, 0.0959234415869435), 
           c(0.204003362529274, 0.0842773551889733, 0.135934577485402, 0.535482053988039, 0.0403026508083115), 
           c(0.329588944612664, 0.26041709070064, 0.075809023616589, 0.0977743545237017, 0.236410586546405), 
           c(0.398958074667906, 0.230595529049823, 0.0619723059947869, 0.0758467028592914, 0.232627387428193), 
           c(0.403577796305832, 0.132670543119795, 0.075388661589732, 0.0881607572726197, 0.300202241712021), 
           c(0.417342909717127, 0.249422976906781, 0.107277535634155, 0.135973187031101, 0.0899833907108366), 
           c(0.271886310519583, 0.209612690795878, 0.0430383618392219, 0.0470214723736321, 0.428441164471686), 
           c(0.603083332521814, 0.162136630097334, 0.0331922876605594, 0.0354249527498347, 0.166162796970458), 
           c(0.227334986499642, 0.293302343803352, 0.235275290171967, 0.0763123102716286, 0.167775069253411), 
           c(0.552073847825097, 0.0936981495460931, 0.201799621203427, 0.0457562943857287, 0.106672087039655), 
           c(0.142766297938479, 0.128473798979268, 0.385799523247887, 0.301268151212321, 0.0416922286220445), 
           c(0.610093865832537, 0.0672455383795189, 0.0759321954117185, 0.173892969163789, 0.0728354312124361), 
           c(0.122453185860168, 0.430677675133424, 0.0949120731171025, 0.323412530843117, 0.0285445350461883),
           c(0.1806026455259, 0.130366201390084, 0.0402464563362021, 0.0504308855799309, 0.598353811167882), 
           c(0.721551369637551, 0.105334837972324, 0.030414843377389, 0.0362927619729929, 0.106406187039743)))
})


# library(PPMdecay)
# 
# m <- new_model(alphabet = c("a", "b", "c", "d", "r"), 
#                max_order_bound = 10)
# lapply(0:1, function(i) {
#   predict_seq(m,
#               c("a", "b", "r", "a", "c", "a", "d", "a", "b", "r", "a"),
#               time = 0:10 + i * 11,
#               save_distribution = TRUE,
#               options = ppm_options(decay = decay_buffer(
#                 buffer_time = 3, 
#                 buffer_items = 2, 
#                 buffer_rate = 0.8,
#                 stm_half_life = 2,
#                 stm_rate = 0.3, 
#                 ltm_rate = 0.05, 
#                 noise = 0
#               )))
# }) %>% do.call(rbind, .) %>% `$`(distribution) %>%
#   lapply(function(x) magrittr::set_names(x, NULL)) %>% 
#   {dump(".", file = "")}

test_that("no decay", {
  abra <- c("a", "b", "r", "a", "c", "a", "d", "a", "b", "r", "a") %>% 
    factor() %>% 
    as.integer() %>% 
    magrittr::subtract(1L)
  
  m <- new_ppm_decay(alphabet_size = 5L, 
                     order_bound = 10L,
                     buffer_length_time = 20, 
                     buffer_length_items = 200, 
                     buffer_weight = 1,
                     stm_half_life = 2,
                     stm_weight = 1, 
                     ltm_weight = 1, 
                     noise = 0)
  lapply(0:1, function(i) {
    model_seq(m, abra, time = 0:10 + i * 11)
  }) %>% do.call(rbind, .) %>% 
    `$`(distribution) %>% 
    expect_equal(
      list(c(0.2, 0.2, 0.2, 0.2, 0.2), 
           c(0.636363636363636, 0.0909090909090909, 0.0909090909090909, 0.0909090909090909, 0.0909090909090909), 
           c(0.411764705882353, 0.411764705882353, 0.0588235294117647, 0.0588235294117647, 0.0588235294117647), 
           c(0.304347826086957, 0.304347826086957, 0.0434782608695652, 0.0434782608695652, 0.304347826086957), 
           c(0.220338983050847, 0.627118644067797, 0.0169491525423729, 0.0169491525423729, 0.11864406779661), 
           c(0.371428571428571, 0.2, 0.2, 0.0285714285714286, 0.2), 
           c(0.152, 0.392, 0.392, 0.008, 0.056), 
           c(0.404255319148936, 0.148936170212766, 0.148936170212766, 0.148936170212766, 0.148936170212766), 
           c(0.116279069767442, 0.283720930232558, 0.283720930232558, 0.283720930232558, 0.0325581395348837), 
           c(0.104602510460251, 0.0543933054393305, 0.0292887029288703, 0.0292887029288703, 0.782426778242678), 
           c(0.924098671726755, 0.0246679316888046, 0.0132827324478178, 0.0132827324478178, 0.0246679316888046), 
           c(0.436619718309859, 0.183098591549296, 0.0985915492957747, 0.0985915492957747, 0.183098591549296), 
           c(0.0951156812339331, 0.434447300771208, 0.218508997429306, 0.218508997429306, 0.0334190231362468), 
           c(0.0490066225165563, 0.0251655629139073, 0.00927152317880795, 0.00927152317880795, 0.907284768211921), 
           c(0.978592013174146, 0.00782214903252368, 0.00288184438040346, 0.00288184438040346, 0.00782214903252368), 
           c(0.00933362274799218, 0.0666377252007814, 0.897547210766225, 0.0223572823963534, 0.00412415888864771), 
           c(0.982224946368373, 0.00582286239656757, 0.00398406374501992, 0.00214526509347227, 0.00582286239656757), 
           c(0.00202554669091811, 0.0141788268364268, 0.00946633045347443, 0.973543879955355, 0.00078541606382539), 
           c(0.995613734493866, 0.00130217257213351, 0.000890960180933452, 0.000890960180933452, 0.00130217257213351), 
           c(0.000447594788369046, 0.995279909504472, 0.00205893602649761, 0.00205893602649761, 0.000154623654163852), 
           c(0.000213139466841312, 9.68815758369599e-05, 5.03784194352192e-05, 5.03784194352192e-05, 0.999589222118451), 
           c(0.999929717027807, 2.31193987476684e-05, 1.20220873487876e-05, 1.20220873487876e-05, 2.31193987476684e-05))
    )
  
  # m <- new_model(alphabet = c("a", "b", "c", "d", "r"), 
  #                max_order_bound = 10)
  # v1 <- lapply(0:1, function(i) {
  #   predict_seq(m,
  #               c("a", "b", "r", "a", "c", "a", "d", "a", "b", "r", "a"),
  #               time = 0:10 + i * 11,
  #               save_distribution = TRUE,
  #               options = ppm_options(decay = decay_buffer(
  #                 buffer_time = 200, 
  #                 buffer_items = 200, 
  #                 buffer_rate = 1,
  #                 stm_half_life = 2,
  #                 stm_rate = 1, 
  #                 ltm_rate = 1, 
  #                 noise = 0
  #               )))
  # }) %>% do.call(rbind, .) %>%
  #   (function(x) {
  #     x$distribution <- lapply(x$distribution, function(x) set_names(x, NULL))
  #     x
  #   }) %>% 
  #   `$`(distribution) %>% 
  #   {dump(".", file = "")}
  
})

if (require("PPMdecay")) {
  compare_implementations <- function(order_bound = 10L,
                                      buffer_length_time = 200, 
                                      buffer_length_items = 200, 
                                      buffer_weight = 1,
                                      stm_half_life = 2,
                                      stm_weight = 1, 
                                      ltm_weight = 1, 
                                      noise = 0) {
    abra <- c("a", "b", "r", "a", "c", "a", "d", "a", "b", "r", "a") %>% 
      factor() %>% 
      as.integer() %>% 
      magrittr::subtract(1L)    
    
    m1 <- new_ppm_decay(alphabet_size = 5,
                        order_bound = order_bound,
                        buffer_length_time = buffer_length_time,
                        buffer_length_items = buffer_length_items,
                        buffer_weight = buffer_weight,
                        stm_half_life = stm_half_life,
                        stm_weight = stm_weight,
                        ltm_weight = ltm_weight,
                        noise = noise)
    res1 <- lapply(0:1, function(i) {
      model_seq(m1, abra, time = 0:10 + i * 11)
    }) %>% do.call(rbind, .)
    
    m2 <- new_model(alphabet = c("a", "b", "c", "d", "r"), 
                    max_order_bound = 10)
    res2 <- lapply(0:1, function(i) {
      predict_seq(m2,
                  c("a", "b", "r", "a", "c", "a", "d", "a", "b", "r", "a"),
                  time = 0:10 + i * 11,
                  save_distribution = TRUE,
                  options = ppm_options(
                    order_bound = order_bound,
                    decay = decay_buffer(
                      buffer_time = buffer_length_time, 
                      buffer_items = buffer_length_items, 
                      buffer_rate = buffer_weight,
                      stm_half_life = stm_half_life,
                      stm_rate = stm_weight, 
                      ltm_rate = ltm_weight, 
                      noise = noise
                    )))
    }) %>% do.call(rbind, .) %>%
      (function(x) {
        x$distribution <- lapply(x$distribution, function(x) set_names(x, NULL))
        x
      })
    
    expect_equal(res1$distribution, 
                 res2$distribution)
  }
  
  compare_implementations()
  compare_implementations(buffer_length_items = 3, 
                          stm_weight = 0,
                          ltm_weight = 0)
  compare_implementations(buffer_length_items = 4, 
                          stm_weight = 0.5,
                          ltm_weight = 0)
  compare_implementations(buffer_length_items = 4, 
                          stm_weight = 0.5,
                          ltm_weight = 0.25)
  compare_implementations(buffer_length_time = 4, 
                          stm_weight = 0.5,
                          ltm_weight = 0.25)
  compare_implementations(buffer_length_time = 7, 
                          buffer_weight = 0.75,
                          stm_weight = 0.5,
                          ltm_weight = 0.25)
  compare_implementations(buffer_length_time = 4, 
                          stm_weight = 0.5,
                          ltm_weight = 0.25,
                          stm_half_life = 4.5)
  compare_implementations(buffer_length_time = 4, 
                          stm_weight = 0.5,
                          ltm_weight = 0.25,
                          stm_half_life = 4.5,
                          order_bound = 3)
}
