package com.treinamaisapi.entity.tema;
import com.treinamaisapi.entity.Concurso;
import com.treinamaisapi.entity.capitulo.Capitulo;
import com.treinamaisapi.entity.pacotes.Pacote;
import com.treinamaisapi.entity.questoes.Questao;
import jakarta.persistence.*;
import lombok.*;

import java.util.ArrayList;
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

    private String nome; // Ex: "Matemática"

    @OneToMany(mappedBy = "tema", cascade = CascadeType.ALL)
    private List<Capitulo> capitulos = new ArrayList<>();

    @ManyToMany(mappedBy = "temas")
    @Builder.Default
    private List<Pacote> pacotes = new ArrayList<>();


}