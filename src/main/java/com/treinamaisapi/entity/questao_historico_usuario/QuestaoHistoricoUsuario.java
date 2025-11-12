package com.treinamaisapi.entity.questao_historico_usuario;

import com.treinamaisapi.entity.enums.NivelDificuldade;
import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.usuarios.Usuario;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Entity
@Table(name="questao_historico_usuario", indexes = {
        @Index(name = "idx_usuario_questao", columnList = "usuario_id, questao_id")
})
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class QuestaoHistoricoUsuario {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "usuario_id", nullable = false)
    private Usuario usuario;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "questao_id", nullable = false)
    private Questao questao;

    private LocalDateTime data;

    private Boolean acertou;

    @Column(name = "simulado_id", nullable = true)
    private Long simuladoId;

    @Enumerated(EnumType.STRING)
    private NivelDificuldade nivelDificuldade;
}
