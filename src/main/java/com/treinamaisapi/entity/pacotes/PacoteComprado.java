package com.treinamaisapi.entity.pacotes;

import com.treinamaisapi.entity.usuarios.Usuario;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.CreationTimestamp;

import java.time.LocalDateTime;

@Builder
@Getter
@Setter
@AllArgsConstructor
@Entity
@Table(name = "pacotes_comprados",
        uniqueConstraints = @UniqueConstraint(columnNames = {"usuario_id", "pacote_id"}))
public class PacoteComprado {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    @JoinColumn(name = "usuario_id")
    private Usuario usuario;

    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    @JoinColumn(name = "pacote_id")
    private Pacote pacote;

    @Column(nullable = false)
    private LocalDateTime dataCompra= LocalDateTime.now();

    @Column(nullable = false)
    private boolean ativo = true;

    @Column(nullable = true)
    private LocalDateTime dataExpiracao;

    public PacoteComprado() {}



    public boolean isExpirado() {
        return dataExpiracao != null && dataExpiracao.isBefore(LocalDateTime.now());
    }
}
