package com.treinamaisapi.entity.tema;
import com.treinamaisapi.entity.questoes.Questao;
import jakarta.persistence.*;
import lombok.*;
import java.util.List;

@Entity
@Table(name = "TEMAS")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Tema {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false, unique = true)
    private String nome;

    private String descricao;

    @OneToMany(mappedBy = "tema", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<Questao> questoes;
}