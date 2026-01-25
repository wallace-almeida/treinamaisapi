package com.treinamaisapi.repository.custom;

import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;

import java.util.List;

public interface QuestaoRepositoryCustom {
    List<Long> findIdsByFiltro(CriarSimuladoRequest filtro);
}

