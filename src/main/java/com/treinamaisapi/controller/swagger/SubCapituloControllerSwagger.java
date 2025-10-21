package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.questao.request.SubcapituloRequest;
import com.treinamaisapi.common.dto.questao.response.SubcapituloResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

@Tag(name = "Sub Capitulo", description = "Usuarios do sistema")
public interface SubCapituloControllerSwagger {


    @PostMapping("/create")
    ResponseEntity<SubcapituloResponse> criar(@RequestBody SubcapituloRequest request);

    @GetMapping("/capitulo/{capituloId}")
    ResponseEntity<List<SubcapituloResponse>> listarPorCapitulo(@PathVariable Long capituloId);
}
