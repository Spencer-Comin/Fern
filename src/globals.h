//
// Created by Spencer Comin on 2021-03-10.
//

#ifndef FERN_GLOBALS_H
#define FERN_GLOBALS_H

#define DEBUG

template<class... Ts>
struct overload : Ts ... {
    using Ts::operator()...;
};
template<class... Ts> overload(Ts...) -> overload<Ts...>;

#endif //FERN_GLOBALS_H
