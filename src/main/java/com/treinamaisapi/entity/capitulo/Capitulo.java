package com.treinamaisapi.entity.capitulo;

import com.treinamaisapi.entity.subCapitulo.Subcapitulo;
import com.treinamaisapi.entity.tema.Tema;
import jakarta.persistence.*;
import lombok.*;

import java.util.ArrayList;
import java.util.List;

@Entity
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "capitulo")
@Builder
public class Capitulo {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String nome; // Ex: "Equações"

    @ManyToOne
    @JoinColumn(name = "tema_id")
    private Tema tema;

    @OneToMany(mappedBy = "capitulo", cascade = CascadeType.ALL)
    private List<Subcapitulo> subcapitulos = new ArrayList<>();
}
