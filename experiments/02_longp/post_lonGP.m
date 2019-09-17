
SELECTED = zeros(200,11);

for idx = 1:200
  fn = ['longp/results/res',num2str(idx),'/Results/1/state.mat'];
  load(fn);
  SELECTED(idx,:) = currVarFlagArr;
end

SELECTED = SELECTED(:,[11,1:10]);

csvwrite('post/selected_lonGP.txt', SELECTED)
