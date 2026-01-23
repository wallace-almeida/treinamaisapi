package com.treinamaisapi.common.filtroAuxil;

import com.treinamaisapi.entity.enums.NivelDificuldade;

public interface NivelPorPacoteProjection {
    Long getPacoteId();
    NivelDificuldade getNivel();
}
