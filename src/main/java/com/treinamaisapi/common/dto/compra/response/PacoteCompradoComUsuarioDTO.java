package com.treinamaisapi.common.dto.compra.response;

import lombok.*;

import java.time.LocalDateTime;

@Data
@NoArgsConstructor
@AllArgsConstructor
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

    private Long concursoId;
}
