package com.treinamaisapi.common.dto.compra.response;

import lombok.*;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PacoteCompradoComUsuarioDTO {

    // Dados do pacote
    private Long pacoteId;
    private String nomePacote;
    private LocalDateTime dataCompra;
    private LocalDateTime dataExpiracao;
    private boolean ativo;

    // Dados do usuário
    private Long usuarioId;
    private String nomeUsuario;
    private String emailUsuario;

    // Dados do concurso
    private Long concursoId;
    private String nomeConcurso;
    private LocalDate dataDaProva;

    // Cálculo dinâmico
    private long diasRestantes;
}
