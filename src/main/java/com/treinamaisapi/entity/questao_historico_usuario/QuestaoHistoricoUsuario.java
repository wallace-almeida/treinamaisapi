package com.treinamaisapi.entity.questao_historico_usuario;

import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.usuarios.Usuario;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Entity
@Table(name="questao_historico_usuario")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class QuestaoHistoricoUsuario {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    private Usuario usuario;

    @ManyToOne
    private Questao questao;

    private LocalDateTime data;

    private Boolean acertou;
}
