package com.treinamaisapi.entity.pacotes;

import com.treinamaisapi.entity.enums.pacotes.MeioPagamento;
import com.treinamaisapi.entity.enums.pacotes.StatusCompra;
import com.treinamaisapi.entity.usuarios.Usuario;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.CreationTimestamp;

import java.time.LocalDateTime;

@Entity
@Table(name = "pacotes_comprados")
@Getter @Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PacoteComprado {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    // ===== RELAÇÕES =====
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "usuario_id")
    private Usuario usuario;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "pacote_id")
    private Pacote pacote;

    // ===== COMPRA =====
    @CreationTimestamp
    @Column(nullable = false, updatable = false)
    private LocalDateTime dataCompra;

    private LocalDateTime dataExpiracao;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private StatusCompra status;

    // ===== PAGAMENTO =====
    @Enumerated(EnumType.STRING)
    private MeioPagamento meioPagamento;

    private String gateway;// ASAAS, MERCADOPAGO, etc

    @Column(unique = true)
    private String pixTxId;// txid PIX
    private LocalDateTime pixExpiracao;

    // ===== CONTROLE =====
    @Column(nullable = false)
    private boolean ativo;

    private LocalDateTime dataCancelamento;
    private String motivoCancelamento;

    // ===== REGRAS =====
    public boolean isExpirado() {
        return dataExpiracao != null &&
                dataExpiracao.isBefore(LocalDateTime.now());
    }

    public boolean possuiAcessoAtivo() {
        return status == StatusCompra.APROVADA && !isExpirado() && ativo;
    }

    public boolean podeCancelar() {
        if (status == StatusCompra.CANCELADA ||
                status == StatusCompra.REEMBOLSADA ||
                status == StatusCompra.EXPIRADA) {
            return false;
        }
        return dataCompra.plusDays(7).isAfter(LocalDateTime.now());
    }
}
