package com.treinamaisapi.common.dto.simulado.filtro;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CapituloFiltroDTO {
    private Long id;
    private String nome;
    private List<SubcapituloFiltroDTO> subcapitulos;
}