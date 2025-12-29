package com.treinamaisapi.service.avatar;


import com.treinamaisapi.entity.avatar.Avatar;
import com.treinamaisapi.repository.AvatarRepository;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class AvatarSeed {

    private final AvatarRepository avatarRepository;

    @PostConstruct
    public void seed() {

        if (avatarRepository.count() > 0) return;

        avatarRepository.saveAll(List.of(

                Avatar.builder()
                        .nome("DEFAULT_1")
                        .caminhoImagem("/avatars/default_1.png")
                        .ativo(true)
                        .build(),

                Avatar.builder()
                        .nome("STUDENT_1")
                        .caminhoImagem("/avatars/student_1.png")
                        .ativo(true)
                        .build(),

                Avatar.builder()
                        .nome("TECH_1")
                        .caminhoImagem("/avatars/tech_1.png")
                        .ativo(true)
                        .build()
        ));
    }
}
