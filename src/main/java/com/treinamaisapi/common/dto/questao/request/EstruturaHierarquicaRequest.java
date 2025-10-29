package com.treinamaisapi.common.dto.questao.request;

import lombok.Data;
import java.util.List;


@Data
public class EstruturaHierarquicaRequest {
    private String nome;
    private List<CapituloLoteRequest> capitulos;
}