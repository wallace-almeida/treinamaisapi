package com.treinamaisapi.common.dto.simulado.request;


import lombok.Data;

import java.util.List;

@Data
public class RespostaSimuladoRequest {
    private Long simuladoId;
    private List<RespostaQuestaoSimulado> respostas;
}
