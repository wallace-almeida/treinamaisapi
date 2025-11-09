package com.treinamaisapi.common.dto.simulado.filtro;

import lombok.*;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PacoteFiltroSimuladoDTO {

    private Long pacoteId;
    private String nomePacote;
    private Long concursoId;
    private String nomeConcurso;
    private Integer versao;

    private List<TemaFiltroDTO> temas;
    private List<String> bancasDisponiveis;
    private List<String> niveisDisponiveis;
}
