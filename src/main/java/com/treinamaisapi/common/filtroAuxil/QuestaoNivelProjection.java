package com.treinamaisapi.common.filtroAuxil;

import com.treinamaisapi.entity.enums.NivelDificuldade;

public interface QuestaoNivelProjection {
    Long getId();
    NivelDificuldade getNivel();
}
