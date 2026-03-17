package com.treinamaisapi.common.dto.admin.compra;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@Builder
public class CompraAdminDTO {

    private Long compraId;

    private Long usuarioId;
    private String nomeUsuario;
    private String emailUsuario;

    private Long pacoteId;
    private String nomePacote;

    private BigDecimal precoOriginal;
    private BigDecimal precoFinal;
    private BigDecimal valorDesconto;

    private String cupom;

    private String status;
    private String meioPagamento;

    private LocalDateTime dataCompra;
    private LocalDateTime dataExpiracao;

    private boolean ativo;

}
